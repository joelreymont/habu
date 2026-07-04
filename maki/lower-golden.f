\ maki/lower-golden.f - device-vs-host GOLDEN for lowered fusion regions (slices 1 + 2).
\
\ CAD-PLAN section 11. LOWER-GOLDEN runs the HOST model-IR executor (maki/executor.f
\ EX-RUN) and the DEVICE region kernel (maki/lower-launch.f) on the SAME deterministic
\ synthetic inputs (maki/golden-artifact.f GA-BIND-SYNTH) and compares the two outputs
\ element-wise. The region's class picks the launch shape and the tolerance:
\   pure elementwise  -> LLA-RUN  (flat kernel), f32 activation tolerance.
\   row-reduce (+ EW) -> LRED-RUN (block-per-row), reduction tolerance.
\
\ Comparison semantics (the host computes f64, the device computes f32): the device value
\ is compared against the host value ROUNDED TO THE f32 GRID (F64>F32 then back to f64),
\ under the section 11 tolerance |dev - host_f32| <= atol + rtol*|host_f32|. Rounding the
\ host to f32 first removes the double-vs-single representation gap so the tolerance measures
\ real kernel error, not the dtype step.
\
\ Tolerance policy (per-class, CAD-PLAN section 11 + the op-registry NUM-* rows). atol is
\ 1e-6 for both. rtol is chosen by op CLASS, because the registry marks activations AND
\ reductions NUM-RELTOL but the reduction class also carries ACC-F32 accumulation over up
\ to k=256 lanes plus (softmax) an ex2.approx step, so its relative error is deeper:
\   elementwise/activation class: rtol = 1e-5 (slice 1; proven for the gelu ex2.approx path).
\   row-reduce class:             rtol = 1e-4. Worst case f32 sum error over k<=256 terms is
\                                 ~ k * 2^-24 ~= 1.5e-5, and ex2.approx adds ~1 ULP on the
\                                 softmax exp; 1e-4 keeps ~6x headroom over that bound.
\ The registry rows imply this: NUM-EXACT would be atol-only, NUM-ULP a few ULP; NUM-RELTOL
\ is a relative band that must scale with the op class's accumulation depth (the ACC-F32 rows).
\
\ Verdict: V-PASS (all within tolerance), V-FAIL (first offending element named), or
\ V-NOTRUN (off-device: no libcuda). LOWER-GOLDEN-REASON$ carries the one-line reason.
\ maki -> habu only; verdict-only (no owned throw codes).

require lib/errors.f
require lib/string.f
require lib/float.f
require lib/fmt.f
require src/arch/ptx/emit.f
require lib/ptx/cg.f
require maki/report.f
require maki/op-registry.f
require maki/model-ir.f
require maki/fusion-plan.f
require maki/executor.f
require maki/golden-artifact.f
require maki/lower-launch.f

package MAKI

-6 constant LG-ATOL-EXP        \ atol = 10^-6 (both classes, CAD-PLAN section 11 f32)
-5 constant LG-RTOL-EW         \ elementwise/activation class rtol = 10^-5 (slice 1)
-4 constant LG-RTOL-RED        \ row-reduce class rtol = 10^-4 (f32 accumulation + ex2.approx)

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
: LG-WITHIN? ( r r n n -- bool ) {: dev:r host:r ae:n re:n :}
   ae POW10  re POW10 host fabs f* f+ {: tol:r :}
   dev host f- fabs {: d:r :}
   tol d f< 0= ;                                \ pass iff |dev-host| <= tol

variable LG-BADI                                \ first mismatched element index (-1 = none)
: LG-COMPARE ( n n -- bool ) {: ae:n re:n :}
   LLA-OUT-NODE@ EX-OUT@ {: hp:ptr :}
   LLA-ELEMS@ {: n:n :}
   -1 LG-BADI !
   n 0 ?do
      i LLA-OUT@  hp i T-GET LG-NARROW  ae re LG-WITHIN? 0= if
         i LG-BADI !  false unloop exit
      then
   loop
   LG-BADI @ 0 < ;

: LG-PASS-REASON ( n -- ) {: rid:n :}
   LG-RE-RESET
   s" lower-golden: REGION_" LG-RE+ rid LG-RE-INT
   s"  device==host within f32 tol (" LG-RE+ LLA-ELEMS@ LG-RE-INT s"  elems)" LG-RE+ ;
: LG-FAIL-REASON ( n -- ) {: rid:n :}
   LG-RE-RESET
   s" lower-golden: REGION_" LG-RE+ rid LG-RE-INT
   s"  mismatch beyond f32 tol at elem " LG-RE+ LG-BADI @ LG-RE-INT ;

\ region class -> launch shape + tolerance (a reduction bit routes to the row kernel)
: LG-REDUCE? ( n -- bool )  FP-REGION-CLASSMIX  1 CLASS-ROW-REDUCE lshift  and  0= 0= ;

public
\ LOWER-GOLDEN ( rid -- verdict ). Requires the region's cubin already assembled and its
\ path set via LLA-CUBIN! (the device tool does emit+ptxas first).
: LOWER-GOLDEN ( n -- n ) {: rid:n :}
   CUDA:OPEN? 0= if
      LG-RE-RESET s" lower-golden: off-device (libcuda unavailable)" LG-RE+  V-NOTRUN exit then
   GA-BIND-SYNTH                                \ bind + fill synthetic inputs (host + device share them)
   MIR-N@ EX-RUN-N                              \ host reference
   rid LG-REDUCE? if  rid LRED-RUN  LG-RTOL-RED  else  rid LLA-RUN  LG-RTOL-EW  then {: re:n :}
   LG-ATOL-EXP re LG-COMPARE if rid LG-PASS-REASON V-PASS else rid LG-FAIL-REASON V-FAIL then ;

end-package
