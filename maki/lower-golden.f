\ maki/lower-golden.f - the FIRST device-vs-host GOLDEN of the CAD plan (slice 1).
\
\ CAD-PLAN section 11. LOWER-GOLDEN runs the HOST model-IR executor (maki/executor.f
\ EX-RUN) and the DEVICE region kernel (maki/lower-launch.f LLA-RUN) on the SAME
\ deterministic synthetic inputs (maki/golden-artifact.f GA-BIND-SYNTH) and compares
\ the two outputs element-wise.
\
\ Comparison semantics (the host computes f64, the device computes f32): the device
\ value is compared against the host value ROUNDED TO THE f32 GRID (F64>F32 then back
\ to f64), under the section 11 f32 tolerance |dev - host_f32| <= atol + rtol*|host_f32|
\ with atol 1e-6, rtol 1e-5. Rounding the host to f32 first removes the double-vs-single
\ representation gap so the tolerance measures real kernel error (f32 rounding along the
\ chain + ex2.approx on the transcendental path), not the dtype step.
\
\ Verdict: V-PASS (all elements within tolerance), V-FAIL (a mismatch, reason names the
\ first offending element), or V-NOTRUN (off-device: no libcuda). LOWER-GOLDEN-REASON$
\ carries the one-line reason. maki -> habu only; verdict-only (no owned throw codes).

require lib/errors.f
require lib/string.f
require lib/float.f
require lib/fmt.f
require src/arch/ptx/emit.f
require lib/ptx/cg.f
require maki/report.f
require maki/model-ir.f
require maki/executor.f
require maki/golden-artifact.f
require maki/lower-launch.f

package MAKI

-6 constant LG-ATOL-EXP        \ atol = 10^-6 (CAD-PLAN section 11 f32 default)
-5 constant LG-RTOL-EXP        \ rtol = 10^-5

\ ---- reason buffer ---------------------------------------------------------
128 constant LG-RE-CAP
create LG-RE LG-RE-CAP allot  variable LG-RE-U
: LG-RE-RESET ( -- )  0 LG-RE-U ! ;
: LG-RE+ ( ptr u8 n -- ) {: a:ptr u:n :}
   LG-RE-U @ u + LG-RE-CAP > if exit then
   a LG-RE LG-RE-U @ + u BYTE-COPY  LG-RE-U @ u + LG-RE-U ! ;
: LG-RE-INT ( n -- )  SB-RESET SB-INT SB$ LG-RE+ ;
public
: LOWER-GOLDEN-REASON$ ( -- ptr u8 n )  LG-RE LG-RE-U @ ;
private

\ ---- tolerance comparison (device f32 vs host rounded to the f32 grid) -------
: LG-NARROW ( r -- r )  F64>F32 F32>F64 ;      \ round a host f64 onto the f32 grid
: LG-WITHIN? ( r r -- bool ) {: dev:r host:r :}
   LG-ATOL-EXP POW10  LG-RTOL-EXP POW10 host fabs f* f+ {: tol:r :}
   dev host f- fabs {: d:r :}
   tol d f< 0= ;                                \ pass iff |dev-host| <= tol

variable LG-BADI                                \ first mismatched element index (-1 = none)
: LG-COMPARE ( -- bool )
   LEW-OUT-NODE@ EX-OUT@ {: hp:ptr :}
   LEW-ELEMS {: n:n :}
   -1 LG-BADI !
   n 0 ?do
      i LLA-OUT@  hp i T-GET LG-NARROW  LG-WITHIN? 0= if
         i LG-BADI !  false unloop exit
      then
   loop
   LG-BADI @ 0 < ;

: LG-PASS-REASON ( n -- ) {: rid:n :}
   LG-RE-RESET
   s" lower-golden: REGION_" LG-RE+ rid LG-RE-INT
   s"  device==host within f32 tol (" LG-RE+ LEW-ELEMS LG-RE-INT s"  elems)" LG-RE+ ;
: LG-FAIL-REASON ( n -- ) {: rid:n :}
   LG-RE-RESET
   s" lower-golden: REGION_" LG-RE+ rid LG-RE-INT
   s"  mismatch beyond f32 tol at elem " LG-RE+ LG-BADI @ LG-RE-INT ;

public
\ LOWER-GOLDEN ( rid -- verdict ). Requires the region's cubin already assembled and
\ its path set via LLA-CUBIN! (the device tool does emit+ptxas first).
: LOWER-GOLDEN ( n -- n ) {: rid:n :}
   CUDA:OPEN? 0= if
      LG-RE-RESET s" lower-golden: off-device (libcuda unavailable)" LG-RE+  V-NOTRUN exit then
   GA-BIND-SYNTH                                \ bind + fill synthetic inputs (host + device share them)
   MIR-N@ EX-RUN-N                              \ host reference
   rid LLA-RUN                                  \ device region launch (analysis + upload + readback)
   LG-COMPARE if rid LG-PASS-REASON V-PASS else rid LG-FAIL-REASON V-FAIL then ;

end-package
