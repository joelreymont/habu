\ swiglu-device-test.f - GB10 device proof of the checked SWIGLU_ROWS forward (silu(gate)*up)
\ and its closed-form VJP: a forward golden vs the maki CPU reference and a finite-difference
\ gradcheck of BOTH input gradients, both on the actual GPU (dot habu-infer-swiglu-op).
\
\ Self-contained, mirrors tools/ptx/rmsnorm-device-test.f: spawns bin/hb to emit the PTX
\ module holding SWIGLU_ROWS (tools/ptx/swiglu-cg.f), ptxas-assembles it to a PRIVATE
\ per-run cubin for the PROBED device arch (sm_121a GB10, ATGT:LABEL$ - never a hardcoded
\ target), loads the cubin and pulls the handle. IDLE-CHECK FIRST: off-device (no libcuda)
\ it records a SKIP and still check-loads. FORWARD GOLDEN: run SWIGLU_ROWS on a fixed row
\ and assert each y[i] matches maki/swiglu.f SWIGLU-F within tolerance. GRADCHECK: SwiGLU has
\ two inputs; for each j perturb gate[j] (then up[j]) by +-eps, re-run the SAME device
\ forward, form the central difference sum_i ct[i]*(y+[i]-y-[i])/(2eps), and assert it
\ matches the host closed-form d_gate = SWIGLU-DGATE / d_up = SWIGLU-DUP - so the device
\ forward AND the closed-form VJP are proven on-device (the VJP itself runs on the host as
\ OP-SILU/OP-MUL/OP-SILU-BWD, so no device backward kernel is needed).
\
\ TOLERANCE (measured on GB10 sm_121a, not defaulted): the forward |device f32 - host f64| is
\ below 5e-7 (under print resolution) over this fixture, and the FD-vs-analytic gradient gap
\ is at most 2.4e-4 (central-difference O(eps^2) truncation at eps=2^-12 plus f32 device noise);
\ SW-NEAR? uses 2e-3, ~8x over the measured worst case. Fully checked Habu via lib/ffi-abi.f.
\ Load after lib/test.f, lib/ffi-abi.f, lib/ptx/cg.f, maki/array.f, maki/swiglu.f.

require lib/errors.f
require lib/string.f
require lib/float.f
require lib/float32.f
require lib/fmt.f
require lib/test.f
require maki/array.f
require maki/swiglu.f
require src/arch/ptx/emit.f
require lib/ptx/cg.f
require lib/ptx/header.f
require lib/ptx/launch.f
require lib/ptx/toolchain.f
require lib/ptx/sentinel.f
require lib/ptx/cuda-driver.f
require lib/ptx/cuda-scope.f
require maki/eval/active-target.f

package SWIGLU-DEVICE-TEST

using F32

private

4 constant SWK
create SW-P1 64 allot
create SW-KF 32 allot
create SW-GIN 16 allot  create SW-UIN 16 allot  create SW-OUT 16 allot    \ f32 device-side packs
create SW-EOUT $8000 allot  create SW-EERR $1000 allot                     \ child emit capture
create SW-QO $1000 allot    create SW-QE $1000 allot                       \ ptxas capture
create HG 4 cells allot  create HU 4 cells allot   create HCT 4 cells allot \ host f64 inputs + cotangent
create HYREF 4 cells allot create HYDEV 4 cells allot                       \ forward golden
create HYP 4 cells allot  create HYM 4 cells allot                          \ perturbed device outputs
create HDGA 4 cells allot create HDGN 4 cells allot                         \ d_gate analytic / numerical
create HDUA 4 cells allot create HDUN 4 cells allot                         \ d_up   analytic / numerical
variable SW-DEV variable SW-CTX variable SW-MF
variable SW-FWD variable SW-dG variable SW-dU variable SW-dO variable SW-KV

: F32! ( n ptr u8 n -- ) {: v buf idx :} idx 4 * {: o :}
   v $FF and buf o + c!  v 8 rshift $FF and buf o 1 + + c!
   v 16 rshift $FF and buf o 2 + + c!  v 24 rshift $FF and buf o 3 + + c! ;
: F32@ ( ptr u8 n -- n ) {: buf idx :} idx 4 * {: o :}
   buf o + c@  buf o 1 + + c@ 8 lshift or  buf o 2 + + c@ 16 lshift or  buf o 3 + + c@ 24 lshift or ;
: PACK4   ( ptr r ptr u8 -- ) {: src:ptr dst:ptr :}  SWK 0 ?do  src i T-GET NARROW  dst i F32!  loop ;
: UNPACK4 ( ptr u8 ptr r -- ) {: src:ptr dst:ptr :}  SWK 0 ?do  src i F32@ WIDEN  dst i T-SET  loop ;
: SW-OUT-GUARD ( -- )  SWK 0 ?do  SW-OUT i F32@ PTXSENT:GUARD drop  loop ;   \ fail closed if copy-back dropped

: SW-DEVICE? ( -- bool )  CUDA:OPEN? ;

\ spawn bin/hb to emit the SWIGLU_ROWS module (swiglu-cg.f) to the private PTX
: SW-EMIT-WRITE ( len len rc -- n ) {: o:len e:len c:rc :}
   SW-EERR e LEN>N  c RC>N  PTXTC:EMIT-GUARD
   PTXTC:PTX$ SW-EOUT o LEN>N WRITE-ALL  o LEN>N ;
: SW-EMIT ( -- n )
   PROC-ARGV-RESET
   s" --load"                 >LEN PROC-ARGV+
   s" tools/ptx/swiglu-cg.f"  >LEN PROC-ARGV+
   s" bin/hb" >LEN  SW-EOUT $8000 >LEN  SW-EERR $1000 >LEN  20000 >MS  RUN-ARGV-CAPTURE
   MATCH result
     ok  OF PCAP-CAPTURED:UNMAKE 0 >RC SW-EMIT-WRITE ENDOF
     err OF PCAP-FAILED:UNMAKE SW-EMIT-WRITE ENDOF
   ;MATCH ;

: SW-PTXAS ( -- n )
   ATGT:LABEL$ PTXTC:TC-ARCH!                        \ assembler arch from the probed active target
   SW-QO $1000 >LEN SW-QE $1000 >LEN PTXTC:ASSEMBLE ;

\ load the cubin and pull the SWIGLU_ROWS handle
: SW-SETUP ( -- )
   CUDA:OPEN
   0 CUDA:CU-INIT CUDA:RC0
   SW-DEV 0 >IDX CUDA:CU-DEVICE-GET CUDA:RC0
   SW-CTX SW-DEV @ >CUDA-DEV CUDA:CU-DEVICE-PRIMARY-CTX-RETAIN CUDA:RC0
   SW-DEV @ >CUDA-DEV CUDA-SCOPE:OWN-PRIMARY-CTX
   SW-CTX @ >CUDA-CTX CUDA:CU-CTX-SET-CURRENT CUDA:RC0
   PTXTC:CUBIN$ SW-P1 FFI:CSTR
   SW-MF SW-P1 CUDA:CU-MODULE-LOAD CUDA:RC0
   SW-MF @ >CUDA-MOD CUDA-SCOPE:OWN-MODULE
   s" SWIGLU_ROWS" SW-KF FFI:CSTR
   SW-FWD SW-MF @ >CUDA-MOD SW-KF CUDA:CU-MODULE-GET-FUNCTION CUDA:RC0
   SW-dG 16 >LEN CUDA:CU-MEM-ALLOC CUDA:RC0   SW-dG @ >CUDA-DEVPTR CUDA-SCOPE:OWN-DEVPTR
   SW-dU 16 >LEN CUDA:CU-MEM-ALLOC CUDA:RC0   SW-dU @ >CUDA-DEVPTR CUDA-SCOPE:OWN-DEVPTR
   SW-dO 16 >LEN CUDA:CU-MEM-ALLOC CUDA:RC0   SW-dO @ >CUDA-DEVPTR CUDA-SCOPE:OWN-DEVPTR
   SWK SW-KV ! ;

\ forward SWIGLU_ROWS on f64 gate/up rows -> f64 output (gate=%rd1 up=%rd2 out=%rd3 k=%r1)
: SW-FWD-RUN ( ptr r ptr r ptr r -- ) {: gsrc:ptr usrc:ptr dst:ptr :}
   SW-OUT 16 PTXSENT:FILL
   1 SWK 256 PTX-ROW-LAUNCH-CHECK
   gsrc SW-GIN PACK4   usrc SW-UIN PACK4
   SW-dG @ >CUDA-DEVPTR SW-GIN 16 >LEN CUDA:HTOD
   SW-dU @ >CUDA-DEVPTR SW-UIN 16 >LEN CUDA:HTOD
   SW-FWD @ >CUDA-FN 256 1 1 CUDA:CU-FUNC-SET-BLOCK-SHAPE CUDA:RC0
   SW-FWD @ >CUDA-FN 28 >LEN CUDA:CU-PARAM-SET-SIZE CUDA:RC0
   SW-FWD @ >CUDA-FN 0 >IDX  SW-dG 8 >LEN CUDA:CU-PARAM-SET-V CUDA:RC0
   SW-FWD @ >CUDA-FN 8 >IDX  SW-dU 8 >LEN CUDA:CU-PARAM-SET-V CUDA:RC0
   SW-FWD @ >CUDA-FN 16 >IDX SW-dO 8 >LEN CUDA:CU-PARAM-SET-V CUDA:RC0
   SW-FWD @ >CUDA-FN 24 >IDX SW-KV 4 >LEN CUDA:CU-PARAM-SET-V CUDA:RC0
   SW-FWD @ >CUDA-FN 1 1 CUDA:CU-LAUNCH-GRID CUDA:RC0
   CUDA:CU-CTX-SYNCHRONIZE CUDA:RC0
   SW-OUT SW-dO @ >CUDA-DEVPTR 16 >LEN CUDA:DTOH
   SW-OUT-GUARD
   SW-OUT dst UNPACK4 ;

: SW-EPS ( -- r )  1.0 4096.0 f/ ;                  \ 2^-12, exact f32
\ numerical d_gate[j] = sum_i ct[i]*(y+[i]-y-[i]) / (2 eps), gate[j] perturbed, up fixed
: SW-NUM-GATE-J ( n -- r ) {: jx :}
   HG jx T-GET {: x0 :}
   x0 SW-EPS f+ HG jx T-SET   HG HU HYP SW-FWD-RUN
   x0 SW-EPS f- HG jx T-SET   HG HU HYM SW-FWD-RUN
   x0 HG jx T-SET
   0.0  SWK 0 ?do  HCT i T-GET  HYP i T-GET HYM i T-GET f-  f*  f+  loop
   SW-EPS 2.0 f* f/ ;
\ numerical d_up[j], up[j] perturbed, gate fixed
: SW-NUM-UP-J ( n -- r ) {: jx :}
   HU jx T-GET {: x0 :}
   x0 SW-EPS f+ HU jx T-SET   HG HU HYP SW-FWD-RUN
   x0 SW-EPS f- HU jx T-SET   HG HU HYM SW-FWD-RUN
   x0 HU jx T-SET
   0.0  SWK 0 ?do  HCT i T-GET  HYP i T-GET HYM i T-GET f-  f*  f+  loop
   SW-EPS 2.0 f* f/ ;

: SW-NEAR? ( r r -- bool ) {: a b :}  a b f- fabs  1.0 500.0 f/ f< ;   \ |a-b| < 2e-3 (measured margin)

\ gate = [-2, -0.5, 0.5, 2], up = [1, -1, 2, 0.5], cotangent = [0.7, -1.3, 0.9, 2.0]
: SW-RUN ( -- )
   -2.0 HG 0 T-SET -0.5 HG 1 T-SET 0.5 HG 2 T-SET 2.0 HG 3 T-SET
    1.0 HU 0 T-SET -1.0 HU 1 T-SET 2.0 HU 2 T-SET 0.5 HU 3 T-SET
    0.7 HCT 0 T-SET -1.3 HCT 1 T-SET 0.9 HCT 2 T-SET 2.0 HCT 3 T-SET
   SWK 0 ?do  HG i T-GET HU i T-GET MAKI:SWIGLU-F  HYREF i T-SET  loop           \ maki CPU forward (f64)
   SWK 0 ?do  HCT i T-GET HG i T-GET HU i T-GET MAKI:SWIGLU-DGATE  HDGA i T-SET  loop  \ analytic d_gate
   SWK 0 ?do  HCT i T-GET HG i T-GET MAKI:SWIGLU-DUP  HDUA i T-SET  loop         \ analytic d_up
   [: SW-SETUP                                        \ acquire+own ctx/module/buffers; scope unwinds on return/throw
      HG HU HYDEV SW-FWD-RUN                          \ device forward golden
      SWK 0 ?do  HYDEV i T-GET  HYREF i T-GET  SW-NEAR?  TTRUE  loop
      SWK 0 ?do  i SW-NUM-GATE-J  HDGN i T-SET  loop  \ numerical d_gate
      SWK 0 ?do  i SW-NUM-UP-J    HDUN i T-SET  loop  \ numerical d_up
   ;] CUDA-SCOPE:SCOPE
   SWK 0 ?do  HDGN i T-GET  HDGA i T-GET  SW-NEAR?  TTRUE  loop             \ d_gate FD == analytic
   SWK 0 ?do  HDUN i T-GET  HDUA i T-GET  SW-NEAR?  TTRUE  loop ;           \ d_up   FD == analytic

: SWIGLU-DEVICE-MAIN ( -- )
   T-RESET
   SW-DEVICE? 0= if
      s" swiglu-device: libcuda.so.1 unavailable -> SWIGLU_ROWS forward golden + FD gradcheck SKIPPED (off-device)" type cr
      T-REPORT exit
   then
   s" habu-ptx-swiglu" PTXTC:PREPARE
   SW-EMIT drop
   SW-PTXAS PTXTC:ASM-REPORT 0 T=
   SW-RUN
   PTXTC:CLEAN
   s" swiglu device: SWIGLU_ROWS forward golden vs maki SWIGLU-F AND both-input finite-difference gradcheck (vs SWIGLU-DGATE/DUP) verified on " type ATGT:LABEL$ type cr
   T-REPORT ;

SWIGLU-DEVICE-MAIN

;using
;package
