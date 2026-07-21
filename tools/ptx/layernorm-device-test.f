\ layernorm-device-test.f - GB10 device proof of the checked LAYERNORM_ROWS forward
\ and its LAYERNORM_BWD_ROWS VJP: a forward golden vs the maki CPU reference, a
\ finite-difference gradcheck of the backward, AND a backward parity check vs the
\ maki closed form - all on the actual GPU.
\
\ Self-contained, mirrors tools/ptx/rmsnorm-device-test.f: spawns bin/hb to emit ONE
\ PTX module holding BOTH LAYERNORM_ROWS and LAYERNORM_BWD_ROWS (tools/ptx/layernorm-cg.f),
\ ptxas-assembles it to a PRIVATE per-run cubin for the PROBED device arch (sm_87
\ Orin / sm_121a GB10, ATGT:LABEL$ - never a hardcoded target), loads that SINGLE
\ cubin, and pulls both handles. FORWARD GOLDEN: run LAYERNORM_ROWS on a fixed row and
\ assert it matches maki/layernorm.f LN-FWD within tolerance. GRADCHECK: perturb each
\ x[j] by +-eps, re-run the SAME device forward, form the central difference
\ sum_i dy[i]*(y+[i]-y-[i])/(2eps), and assert it matches the device backward dx[j].
\ BACKWARD PARITY: assert the device dx[j] also matches the maki LN-BWD closed form.
\ Fully checked Habu via lib/ffi.f. Off-device (no libcuda) it records a SKIP and
\ still check-loads. Load after lib/test.f, lib/ffi.f, lib/ptx/cg.f, maki/array.f.
\
\ AFFINE: only PLAIN LayerNorm is proved here - the kernel pair takes no gamma/beta
\ (tools/ptx/layernorm-cg.f records the affine boundary); the affine parameter grads
\ are cross-row column reductions, a different kernel shape.
\
\ OWED: the dot targets sm_87 goldens on Orin; that box is unavailable here, so the
\ Orin sm_87 golden + its profile row are OWED and recorded in tools/ptx/perf-rows.tsv.

require lib/errors.f
require lib/string.f
require lib/float.f
require lib/fmt.f
require lib/test.f
require maki/array.f
require maki/layernorm.f
require src/arch/ptx/emit.f
require lib/ptx/cg.f
require lib/ptx/header.f
require lib/ptx/launch.f
require lib/ptx/toolchain.f
require lib/ptx/sentinel.f
require lib/ptx/cuda-driver.f
require maki/eval/active-target.f

package LAYERNORM-DEVICE-TEST

private

4 constant RNK
create RN-P1 64 allot
create RN-KF 32 allot   create RN-KB 32 allot
create RN-IN 16 allot   create RN-OUT 16 allot   create RN-DYB 16 allot     \ f32 device-side packs
create RN-EOUT $8000 allot  create RN-EERR $1000 allot                      \ child emit capture
create RN-QO $1000 allot    create RN-QE $1000 allot                        \ ptxas capture
create HX 4 cells allot create HDY 4 cells allot  create HDXA 4 cells allot \ host f64
create HDXN 4 cells allot create HDXH 4 cells allot create HYREF 4 cells allot create HYDEV 4 cells allot
create HYP 4 cells allot create HYM 4 cells allot
variable RN-DEV variable RN-CTX variable RN-MF
variable RN-FWD variable RN-BWD variable RN-dX variable RN-dDY variable RN-dO variable RN-KV

: F32! ( n ptr u8 n -- ) {: v buf idx :} idx 4 * {: o :}
   v $FF and buf o + c!  v 8 rshift $FF and buf o 1 + + c!
   v 16 rshift $FF and buf o 2 + + c!  v 24 rshift $FF and buf o 3 + + c! ;
: F32@ ( ptr u8 n -- n ) {: buf idx :} idx 4 * {: o :}
   buf o + c@  buf o 1 + + c@ 8 lshift or  buf o 2 + + c@ 16 lshift or  buf o 3 + + c@ 24 lshift or ;
: PACK4   ( ptr a ptr u8 -- ) {: src dst :}  RNK 0 ?do  src i T-GET F64>F32  dst i F32!  loop ;
: UNPACK4 ( ptr u8 ptr a -- ) {: src dst :}  RNK 0 ?do  src i F32@ F32>F64  dst i T-SET  loop ;
: RN-OUT-GUARD ( -- )  RNK 0 ?do  RN-OUT i F32@ PTXSENT:GUARD drop  loop ;   \ fail closed if copy-back dropped

: RN-DEVICE? ( -- bool )  CUDA:OPEN? ;

\ spawn bin/hb to emit the combined fwd+bwd module (layernorm-cg.f) to the private PTX
: RN-EMIT-WRITE ( len len rc -- n ) {: o:len e:len c:rc :}
   RN-EERR e LEN>N  c RC>N  PTXTC:EMIT-GUARD
   PTXTC:PTX$ RN-EOUT o LEN>N WRITE-ALL  o LEN>N ;
: RN-EMIT ( -- n )
   PROC-ARGV-RESET
   s" --load"                     >LEN PROC-ARGV+
   s" tools/ptx/layernorm-cg.f"   >LEN PROC-ARGV+
   s" bin/hb" >LEN  RN-EOUT $8000 >LEN  RN-EERR $1000 >LEN  20000 >MS  RUN-ARGV-CAPTURE
   MATCH result
     ok  OF PCAP-CAPTURED:UNMAKE 0 >RC RN-EMIT-WRITE ENDOF
     err OF PCAP-FAILED:UNMAKE RN-EMIT-WRITE ENDOF
   ;MATCH ;

: RN-PTXAS ( -- n )
   ATGT:LABEL$ PTXTC:TC-ARCH!                        \ assembler arch from the probed active target
   RN-QO $1000 >LEN RN-QE $1000 >LEN PTXTC:ASSEMBLE ;

\ load the ONE combined cubin and pull BOTH function handles from the SAME module
: RN-SETUP ( -- )
   CUDA:OPEN
   0 CUDA:CU-INIT CUDA:RC0
   RN-DEV 0 >IDX CUDA:CU-DEVICE-GET CUDA:RC0
   RN-CTX RN-DEV @ >CUDA-DEV CUDA:CU-DEVICE-PRIMARY-CTX-RETAIN CUDA:RC0
   RN-CTX @ >CUDA-CTX CUDA:CU-CTX-SET-CURRENT CUDA:RC0
   PTXTC:CUBIN$ RN-P1 >CSTR
   RN-MF RN-P1 CUDA:CU-MODULE-LOAD CUDA:RC0
   s" LAYERNORM_ROWS" RN-KF >CSTR
   RN-FWD RN-MF @ >CUDA-MOD RN-KF CUDA:CU-MODULE-GET-FUNCTION CUDA:RC0
   s" LAYERNORM_BWD_ROWS" RN-KB >CSTR
   RN-BWD RN-MF @ >CUDA-MOD RN-KB CUDA:CU-MODULE-GET-FUNCTION CUDA:RC0
   RN-dX 16 >LEN CUDA:CU-MEM-ALLOC CUDA:RC0
   RN-dDY 16 >LEN CUDA:CU-MEM-ALLOC CUDA:RC0
   RN-dO 16 >LEN CUDA:CU-MEM-ALLOC CUDA:RC0
   RNK RN-KV ! ;

\ forward LAYERNORM_ROWS on f64 input `src`, f64 output to `dst` (in=%rd1 out=%rd2 k=%r1)
: RN-FWD-RUN ( ptr a ptr a -- ) {: src dst :}
   RN-OUT 16 PTXSENT:FILL
   1 RNK 256 PTX-ROW-LAUNCH-CHECK
   src RN-IN PACK4
   RN-dX @ >CUDA-DEVPTR RN-IN 16 >LEN CUDA:HTOD
   RN-FWD @ >CUDA-FN 256 1 1 CUDA:CU-FUNC-SET-BLOCK-SHAPE CUDA:RC0
   RN-FWD @ >CUDA-FN 20 >LEN CUDA:CU-PARAM-SET-SIZE CUDA:RC0
   RN-FWD @ >CUDA-FN 0 >IDX  RN-dX 8 >LEN CUDA:CU-PARAM-SET-V CUDA:RC0
   RN-FWD @ >CUDA-FN 8 >IDX  RN-dO 8 >LEN CUDA:CU-PARAM-SET-V CUDA:RC0
   RN-FWD @ >CUDA-FN 16 >IDX RN-KV 4 >LEN CUDA:CU-PARAM-SET-V CUDA:RC0
   RN-FWD @ >CUDA-FN 1 1 CUDA:CU-LAUNCH-GRID CUDA:RC0
   CUDA:CU-CTX-SYNCHRONIZE CUDA:RC0
   RN-OUT RN-dO @ >CUDA-DEVPTR 16 >LEN CUDA:DTOH
   RN-OUT-GUARD
   RN-OUT dst UNPACK4 ;

\ backward LAYERNORM_BWD_ROWS: (HX, HDY) -> HDXA  (x=%rd1 dy=%rd2 out=%rd3 k=%r1)
: RN-BWD-RUN ( -- )
   RN-OUT 16 PTXSENT:FILL
   1 RNK 256 PTX-ROW-LAUNCH-CHECK
   HX RN-IN PACK4   HDY RN-DYB PACK4
   RN-dX @ >CUDA-DEVPTR RN-IN 16 >LEN CUDA:HTOD
   RN-dDY @ >CUDA-DEVPTR RN-DYB 16 >LEN CUDA:HTOD
   RN-BWD @ >CUDA-FN 256 1 1 CUDA:CU-FUNC-SET-BLOCK-SHAPE CUDA:RC0
   RN-BWD @ >CUDA-FN 28 >LEN CUDA:CU-PARAM-SET-SIZE CUDA:RC0
   RN-BWD @ >CUDA-FN 0 >IDX  RN-dX 8 >LEN CUDA:CU-PARAM-SET-V CUDA:RC0
   RN-BWD @ >CUDA-FN 8 >IDX  RN-dDY 8 >LEN CUDA:CU-PARAM-SET-V CUDA:RC0
   RN-BWD @ >CUDA-FN 16 >IDX RN-dO 8 >LEN CUDA:CU-PARAM-SET-V CUDA:RC0
   RN-BWD @ >CUDA-FN 24 >IDX RN-KV 4 >LEN CUDA:CU-PARAM-SET-V CUDA:RC0
   RN-BWD @ >CUDA-FN 1 1 CUDA:CU-LAUNCH-GRID CUDA:RC0
   CUDA:CU-CTX-SYNCHRONIZE CUDA:RC0
   RN-OUT RN-dO @ >CUDA-DEVPTR 16 >LEN CUDA:DTOH
   RN-OUT-GUARD
   RN-OUT HDXA UNPACK4 ;

: RN-RELEASE ( -- )
   RN-MF @ >CUDA-MOD CUDA:CU-MODULE-UNLOAD CUDA:RC0
   RN-DEV @ >CUDA-DEV CUDA:CU-DEVICE-PRIMARY-CTX-RELEASE CUDA:RC0 ;

\ numerical dx[j] = sum_i dy[i]*(y+[i]-y-[i]) / (2 eps)
: RN-EPS ( -- r )  1.0 4096.0 f/ ;                  \ 2^-12, exact f32
: RN-NUM-J ( n -- r ) {: jx :}
   HX jx T-GET {: x0 :}
   x0 RN-EPS f+ HX jx T-SET   HX HYP RN-FWD-RUN
   x0 RN-EPS f- HX jx T-SET   HX HYM RN-FWD-RUN
   x0 HX jx T-SET
   0.0  RNK 0 ?do  HDY i T-GET  HYP i T-GET HYM i T-GET f-  f*  f+  loop
   RN-EPS 2.0 f* f/ ;

: RN-NEAR? ( r r -- bool ) {: a b :}  a b f- fabs  1.0 500.0 f/ f< ;   \ |a-b| < 2e-3

\ x = [1, 2, 0.5, 1.5], dy = [0.1, 0.2, 0.3, 0.4]
: RN-RUN ( -- )
   1.0 HX 0 T-SET  2.0 HX 1 T-SET  0.5 HX 2 T-SET  1.5 HX 3 T-SET
   0.1 HDY 0 T-SET 0.2 HDY 1 T-SET 0.3 HDY 2 T-SET 0.4 HDY 3 T-SET
   HX HYREF RNK MAKI:LN-FWD                           \ maki CPU reference (f64)
   HDY HX HDXH RNK MAKI:LN-BWD                         \ maki closed-form backward (f64)
   RN-SETUP
   HX HYDEV RN-FWD-RUN                                \ device forward golden
   RNK 0 ?do  HYDEV i T-GET  HYREF i T-GET  RN-NEAR?  TTRUE  loop
   RNK 0 ?do  i RN-NUM-J  HDXN i T-SET  loop          \ numerical gradient
   RN-BWD-RUN                                         \ analytic gradient -> HDXA
   RN-RELEASE
   RNK 0 ?do  HDXN i T-GET  HDXA i T-GET  RN-NEAR?  TTRUE  loop    \ device bwd vs finite-difference
   RNK 0 ?do  HDXH i T-GET  HDXA i T-GET  RN-NEAR?  TTRUE  loop ; \ device bwd vs maki closed form

: LAYERNORM-DEVICE-MAIN ( -- )
   T-RESET
   RN-DEVICE? 0= if
      s" layernorm-device: libcuda.so.1 unavailable -> LAYERNORM_ROWS golden + LAYERNORM_BWD_ROWS gradcheck SKIPPED (off-device)" type cr
      T-REPORT exit
   then
   s" habu-ptx-layernorm" PTXTC:PREPARE
   RN-EMIT drop
   RN-PTXAS PTXTC:ASM-REPORT 0 T=
   RN-RUN
   PTXTC:CLEAN
   s" layernorm device: LAYERNORM_ROWS forward golden vs maki LN-FWD AND LAYERNORM_BWD_ROWS finite-difference gradcheck + maki LN-BWD parity verified on " type ATGT:LABEL$ type cr
   T-REPORT ;

LAYERNORM-DEVICE-MAIN

;package
