\ rope-device-test.f - GB10 device proof of the checked ROPE_ROWS pair rotation and
\ its ROPE_BWD_ROWS VJP: a forward golden vs the maki CPU reference and a
\ finite-difference gradcheck of the backward, both on the actual GPU.
\
\ Self-contained, mirrors tools/ptx/rmsnorm-device-test.f: spawns bin/hb to emit ONE
\ PTX module holding BOTH ROPE_ROWS and ROPE_BWD_ROWS (tools/ptx/rope-cg.f),
\ ptxas-assembles it for the PROBED device arch (ATGT:LABEL$; sm_87 Orin / sm_121a
\ GB10), loads that SINGLE cubin, and pulls both handles. One row, head_dim 4 = two
\ rotary pairs at angles 0.5 and 1.0; cos/sin arrive pre-broadcast per pair (the
\ table build is a separate op). FORWARD GOLDEN: ROPE_ROWS vs maki/rope.f ROPE-PAIR
\ per pair. GRADCHECK: perturb each x[j] by +-eps (cos/sin FIXED), re-run the SAME
\ device forward, central-difference sum_i dy[i]*(y+[i]-y-[i])/(2eps), and assert it
\ matches the device backward dx[j]. Off-device it records a SKIP and check-loads.
\
\ OWED: the dot targets sm_87 goldens on Orin; that box is unavailable here, so the
\ Orin sm_87 golden + its profile row are OWED (tools/ptx/perf-rows.tsv).

require lib/errors.f
require lib/string.f
require lib/float.f
require lib/fmt.f
require lib/test.f
require maki/array.f
require maki/rope.f
require src/arch/ptx/emit.f
require lib/ptx/cg.f
require lib/ptx/header.f
require lib/ptx/launch.f
require lib/ptx/toolchain.f
require lib/ptx/sentinel.f
require lib/ptx/cuda-driver.f
require lib/ptx/cuda-scope.f
require maki/eval/active-target.f

package ROPE-DEVICE-TEST

private

4 constant ROK
create RO-P1 64 allot
create RO-KF 32 allot   create RO-KB 32 allot
create RO-IN 16 allot                                                       \ f32 device-side pack
create RO-EOUT $8000 allot  create RO-EERR $1000 allot
create RO-QO $1000 allot    create RO-QE $1000 allot
create HX 4 cells allot  create HDY 4 cells allot   create HCOS 4 cells allot
create HSIN 4 cells allot create HDXA 4 cells allot  create HDXN 4 cells allot
create HYREF 4 cells allot create HYDEV 4 cells allot
create HYP 4 cells allot  create HYM 4 cells allot
variable RO-DEV variable RO-CTX variable RO-MF
variable RO-FWD variable RO-BWD variable RO-dX variable RO-dCOS variable RO-dSIN variable RO-dO variable RO-KV

: F32! ( n ptr u8 n -- ) {: v buf idx :} idx 4 * {: o :}
   v $FF and buf o + c!  v 8 rshift $FF and buf o 1 + + c!
   v 16 rshift $FF and buf o 2 + + c!  v 24 rshift $FF and buf o 3 + + c! ;
: F32@ ( ptr u8 n -- n ) {: buf idx :} idx 4 * {: o :}
   buf o + c@  buf o 1 + + c@ 8 lshift or  buf o 2 + + c@ 16 lshift or  buf o 3 + + c@ 24 lshift or ;
: PACK4   ( ptr a ptr u8 -- ) {: src dst :}  ROK 0 ?do  src i T-GET F64>F32  dst i F32!  loop ;
: UNPACK4 ( ptr u8 ptr a -- ) {: src dst :}  ROK 0 ?do  src i F32@ F32>F64  dst i T-SET  loop ;
: RO-OUT-GUARD ( -- )  ROK 0 ?do  RO-IN i F32@ PTXSENT:GUARD drop  loop ;

: RO-DEVICE? ( -- bool )  CUDA:OPEN? ;

: RO-EMIT-WRITE ( len len rc -- n ) {: o:len e:len c:rc :}
   RO-EERR e LEN>N  c RC>N  PTXTC:EMIT-GUARD
   PTXTC:PTX$ RO-EOUT o LEN>N WRITE-ALL  o LEN>N ;
: RO-EMIT ( -- n )
   PROC-ARGV-RESET
   s" --load"               >LEN PROC-ARGV+
   s" tools/ptx/rope-cg.f"  >LEN PROC-ARGV+
   s" bin/hb" >LEN  RO-EOUT $8000 >LEN  RO-EERR $1000 >LEN  20000 >MS  RUN-ARGV-CAPTURE
   MATCH result
     ok  OF PCAP-CAPTURED:UNMAKE 0 >RC RO-EMIT-WRITE ENDOF
     err OF PCAP-FAILED:UNMAKE RO-EMIT-WRITE ENDOF
   ;MATCH ;

: RO-PTXAS ( -- n )
   ATGT:LABEL$ PTXTC:TC-ARCH!
   RO-QO $1000 >LEN RO-QE $1000 >LEN PTXTC:ASSEMBLE ;

\ upload cos/sin (fixed) to device once; alloc all four device buffers
: RO-SETUP ( -- )
   CUDA:OPEN
   0 CUDA:CU-INIT CUDA:RC0
   RO-DEV 0 >IDX CUDA:CU-DEVICE-GET CUDA:RC0
   RO-CTX RO-DEV @ >CUDA-DEV CUDA:CU-DEVICE-PRIMARY-CTX-RETAIN CUDA:RC0
   RO-DEV @ >CUDA-DEV CUDA-SCOPE:OWN-PRIMARY-CTX
   RO-CTX @ >CUDA-CTX CUDA:CU-CTX-SET-CURRENT CUDA:RC0
   PTXTC:CUBIN$ RO-P1 >CSTR
   RO-MF RO-P1 CUDA:CU-MODULE-LOAD CUDA:RC0
   RO-MF @ >CUDA-MOD CUDA-SCOPE:OWN-MODULE
   s" ROPE_ROWS" RO-KF >CSTR
   RO-FWD RO-MF @ >CUDA-MOD RO-KF CUDA:CU-MODULE-GET-FUNCTION CUDA:RC0
   s" ROPE_BWD_ROWS" RO-KB >CSTR
   RO-BWD RO-MF @ >CUDA-MOD RO-KB CUDA:CU-MODULE-GET-FUNCTION CUDA:RC0
   RO-dX 16 >LEN CUDA:CU-MEM-ALLOC CUDA:RC0    RO-dX @ >CUDA-DEVPTR CUDA-SCOPE:OWN-DEVPTR
   RO-dCOS 16 >LEN CUDA:CU-MEM-ALLOC CUDA:RC0  RO-dCOS @ >CUDA-DEVPTR CUDA-SCOPE:OWN-DEVPTR
   RO-dSIN 16 >LEN CUDA:CU-MEM-ALLOC CUDA:RC0  RO-dSIN @ >CUDA-DEVPTR CUDA-SCOPE:OWN-DEVPTR
   RO-dO 16 >LEN CUDA:CU-MEM-ALLOC CUDA:RC0    RO-dO @ >CUDA-DEVPTR CUDA-SCOPE:OWN-DEVPTR
   HCOS RO-IN PACK4  RO-dCOS @ >CUDA-DEVPTR RO-IN 16 >LEN CUDA:HTOD
   HSIN RO-IN PACK4  RO-dSIN @ >CUDA-DEVPTR RO-IN 16 >LEN CUDA:HTOD
   ROK RO-KV ! ;

\ launch a 4-ptr row kernel (fn handle in variable `fnv`) with rd1-input `src`
\ (packed), output -> `dst`. cos/sin already resident from RO-SETUP.
: RO-LAUNCH ( ptr a ptr a ptr n -- ) {: src dst fnv :}
   RO-IN 16 PTXSENT:FILL
   1 ROK 256 PTX-ROW-LAUNCH-CHECK
   src RO-IN PACK4
   RO-dX @ >CUDA-DEVPTR RO-IN 16 >LEN CUDA:HTOD
   fnv @ >CUDA-FN 256 1 1 CUDA:CU-FUNC-SET-BLOCK-SHAPE CUDA:RC0
   fnv @ >CUDA-FN 36 >LEN CUDA:CU-PARAM-SET-SIZE CUDA:RC0
   fnv @ >CUDA-FN 0 >IDX  RO-dX 8 >LEN CUDA:CU-PARAM-SET-V CUDA:RC0
   fnv @ >CUDA-FN 8 >IDX  RO-dCOS 8 >LEN CUDA:CU-PARAM-SET-V CUDA:RC0
   fnv @ >CUDA-FN 16 >IDX RO-dSIN 8 >LEN CUDA:CU-PARAM-SET-V CUDA:RC0
   fnv @ >CUDA-FN 24 >IDX RO-dO 8 >LEN CUDA:CU-PARAM-SET-V CUDA:RC0
   fnv @ >CUDA-FN 32 >IDX RO-KV 4 >LEN CUDA:CU-PARAM-SET-V CUDA:RC0
   fnv @ >CUDA-FN 1 1 CUDA:CU-LAUNCH-GRID CUDA:RC0
   CUDA:CU-CTX-SYNCHRONIZE CUDA:RC0
   RO-IN RO-dO @ >CUDA-DEVPTR 16 >LEN CUDA:DTOH
   RO-OUT-GUARD
   RO-IN dst UNPACK4 ;

: RO-FWD-RUN ( ptr a ptr a -- )  RO-FWD RO-LAUNCH ;
: RO-BWD-RUN ( -- )              HDY HDXA RO-BWD RO-LAUNCH ;

: RO-EPS ( -- r )  1.0 1024.0 f/ ;                   \ 2^-10, exact f32
: RO-NUM-J ( n -- r ) {: jx :}
   HX jx T-GET {: x0 :}
   x0 RO-EPS f+ HX jx T-SET   HX HYP RO-FWD-RUN
   x0 RO-EPS f- HX jx T-SET   HX HYM RO-FWD-RUN
   x0 HX jx T-SET
   0.0  ROK 0 ?do  HDY i T-GET  HYP i T-GET HYM i T-GET f-  f*  f+  loop
   RO-EPS 2.0 f* f/ ;

: RO-NEAR? ( r r -- bool ) {: a b :}  a b f- fabs  1.0 500.0 f/ f< ;   \ golden: |a-b| < 2e-3
: RO-GNEAR? ( r r -- bool ) {: a b :} a b f- fabs  1.0 100.0 f/ f< ;   \ gradcheck: |a-b| < 1e-2

\ pair0 angle 0.5, pair1 angle 1.0 (cos/sin duplicated across each pair)
: RO-REF ( -- )
   HX 0 T-GET HX 1 T-GET HCOS 0 T-GET HSIN 0 T-GET MAKI:ROPE-PAIR  HYREF 1 T-SET HYREF 0 T-SET
   HX 2 T-GET HX 3 T-GET HCOS 2 T-GET HSIN 2 T-GET MAKI:ROPE-PAIR  HYREF 3 T-SET HYREF 2 T-SET ;

: RO-RUN ( -- )
   1.0 HX 0 T-SET  2.0 HX 1 T-SET  3.0 HX 2 T-SET  4.0 HX 3 T-SET
   0.1 HDY 0 T-SET 0.2 HDY 1 T-SET 0.3 HDY 2 T-SET 0.4 HDY 3 T-SET
   0.8775825618903728 HCOS 0 T-SET  0.8775825618903728 HCOS 1 T-SET
   0.5403023058681398 HCOS 2 T-SET  0.5403023058681398 HCOS 3 T-SET
   0.479425538604203  HSIN 0 T-SET  0.479425538604203  HSIN 1 T-SET
   0.8414709848078965 HSIN 2 T-SET  0.8414709848078965 HSIN 3 T-SET
   RO-REF
   [: RO-SETUP                                        \ acquire+own ctx/module/buffers; scope unwinds on return or throw
      HX HYDEV RO-FWD-RUN                             \ device forward golden
      ROK 0 ?do  HYDEV i T-GET  HYREF i T-GET  RO-NEAR?  TTRUE  loop
      ROK 0 ?do  i RO-NUM-J  HDXN i T-SET  loop       \ numerical gradient (x only)
      RO-BWD-RUN                                      \ analytic gradient -> HDXA
   ;] CUDA-SCOPE:SCOPE
   ROK 0 ?do  HDXN i T-GET  HDXA i T-GET  RO-GNEAR?  TTRUE  loop ;

: ROPE-DEVICE-MAIN ( -- )
   T-RESET
   RO-DEVICE? 0= if
      s" rope-device: libcuda.so.1 unavailable -> ROPE_ROWS golden + ROPE_BWD_ROWS gradcheck SKIPPED (off-device)" type cr
      T-REPORT exit
   then
   s" habu-ptx-rope" PTXTC:PREPARE
   RO-EMIT drop
   RO-PTXAS PTXTC:ASM-REPORT 0 T=
   RO-RUN
   PTXTC:CLEAN
   s" rope device: ROPE_ROWS forward golden vs maki ROPE-PAIR AND ROPE_BWD_ROWS finite-difference gradcheck verified on " type ATGT:LABEL$ type cr
   T-REPORT ;

ROPE-DEVICE-MAIN

;package
