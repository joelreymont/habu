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

-6 constant LG-ATOL-EXP        \ atol = 10^-6 (all classes, CAD-PLAN section 11 f32)
-5 constant LG-RTOL-EW         \ elementwise/activation class rtol = 10^-5 (slice 1)
-4 constant LG-RTOL-RED        \ row-reduce class rtol = 10^-4 (f32 accumulation + ex2.approx)
-4 constant LG-RTOL-MM         \ matmul class rtol = 10^-4 (f32 ACC over K<=256; ~K*2^-24 ~= 1.5e-5, ~6x headroom)
-6 constant LG-RTOL-MV         \ movement copy class rtol = 10^-6 (NUM-EXACT: a device f32 copy equals F64>F32(host))

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

\ region class -> launch shape + tolerance. A matmul bit routes to the tiled-GEMM kernel;
\ a reduction bit routes to the row kernel; otherwise the flat elementwise kernel. A region
\ never mixes a contraction with a reduction (maki/fusion-plan.f), so the order is disjoint.
\ region class routing is owned by lower-launch (LOWER-MODEL-RUN reuses it); alias here.
: LG-MATMUL? ( n -- bool )  LLA-REGION-MATMUL? ;
: LG-REDUCE? ( n -- bool )  LLA-REGION-REDUCE? ;

\ a MATERIALIZED movement region (the region's output node is a movement copy, not a fold)
\ routes to the copy-kernel launch; a dissolved-fold region keeps its EW/RED/MM class route.
: LG-MOVE? ( n -- bool )  LLA-REGION-MOVE? ;

public
\ LOWER-GOLDEN ( rid -- verdict ). Requires the region's cubin already assembled and its
\ path set via LLA-CUBIN! (the device tool does emit+ptxas first).
: LOWER-GOLDEN ( n -- n ) {: rid:n :}
   CUDA:OPEN? 0= if
      LG-RE-RESET s" lower-golden: off-device (libcuda unavailable)" LG-RE+  V-NOTRUN exit then
   GA-BIND-SYNTH                                \ bind + fill synthetic inputs (host + device share them)
   MIR-N@ EX-RUN-N                              \ host reference
   rid LG-MOVE? if
      rid LMV-RUN  LG-RTOL-MV
   else rid LG-MATMUL? if
      rid LMM-RUN  LG-RTOL-MM
   else
      rid LG-REDUCE? if  rid LRED-RUN  LG-RTOL-RED  else  rid LLA-RUN  LG-RTOL-EW  then
   then then {: re:n :}
   LG-ATOL-EXP re LG-COMPARE if rid LG-PASS-REASON V-PASS else rid LG-FAIL-REASON V-FAIL then ;

private

\ ======================= whole-model device-vs-host golden (slice 5) =============
\ LOWER-MODEL-GOLDEN runs the WHOLE forward IR on both legs on the same synthetic inputs -
\ the host executor (EX-RUN-N, f64) and the device (LOWER-MODEL-RUN, region-by-region f32
\ with cross-region device buffers) - and compares the FINAL model output element-wise.
\
\ Tolerance composition (accumulate across regions). The device carries f32 at EVERY region
\ boundary (each materialized producer is rounded to f32 before the next region reads it),
\ while the host stays f64 and narrows to the f32 grid ONCE at the end. So the device
\ accumulates a per-region rounding the single-region golden never sees. First-order error
\ propagation gives final relative error ~ SUM of the per-region class rtols and an absolute
\ floor ~ SUM of the per-region atols; SUMMING is the sound upper bound (maxing would
\ understate a deep chain). Per class: EW 1e-5, ROW-REDUCE 1e-4, MATMUL 1e-4, MOVEMENT 1e-6
\ (the same slice 1-4 per-class bounds), atol 1e-6 each.
: MDL-ATOL ( -- r )  MDL-N-REGIONS@ s>f  LG-ATOL-EXP POW10  f* ;
: MDL-RTOL ( -- r )
   MDL-N-EW@  s>f LG-RTOL-EW  POW10 f*
   MDL-N-RED@ s>f LG-RTOL-RED POW10 f* f+
   MDL-N-MM@  s>f LG-RTOL-MM  POW10 f* f+
   MDL-N-MV@  s>f LG-RTOL-MV  POW10 f* f+ ;

\ linear-tolerance compare (the composed tol is a SUM, not a single power of ten)
: LG-WITHIN-LIN? ( r r r r -- bool ) {: dev:r host:r atol:r rtol:r :}
   atol  rtol host fabs f*  f+ {: tol:r :}
   dev host f- fabs {: d:r :}
   tol d f< 0= ;
: LG-COMPARE-LIN ( r r -- bool ) {: atol:r rtol:r :}
   LLA-OUT-NODE@ EX-OUT@ {: hp:ptr :}
   LLA-ELEMS@ {: n:n :}
   -1 LG-BADI !
   n 0 ?do
      i LLA-OUT@  hp i T-GET LG-NARROW  atol rtol LG-WITHIN-LIN? 0= if
         i LG-BADI !  false unloop exit
      then
   loop
   LG-BADI @ 0 < ;

: MDL-PASS-REASON ( -- )
   LG-RE-RESET
   s" lower-model-golden: device==host within composed f32 tol (" LG-RE+ LLA-ELEMS@ LG-RE-INT
   s"  elems, " LG-RE+ MDL-N-REGIONS@ LG-RE-INT s"  regions)" LG-RE+ ;
: MDL-FAIL-REASON ( -- )
   LG-RE-RESET
   s" lower-model-golden: mismatch beyond composed f32 tol at elem " LG-RE+ LG-BADI @ LG-RE-INT ;

public
\ LOWER-MODEL-GOLDEN ( -- verdict ). Requires FP-BUILD + each region's cubin registered
\ (MDL-CUBIN!). Off-device -> V-NOTRUN (reason in LOWER-GOLDEN-REASON$).
: LOWER-MODEL-GOLDEN ( -- n )
   CUDA:OPEN? 0= if
      LG-RE-RESET s" lower-model-golden: off-device (libcuda unavailable)" LG-RE+  V-NOTRUN exit then
   GA-BIND-SYNTH                                \ bind + fill synthetic inputs (host + device share them)
   MIR-N@ EX-RUN-N                              \ host reference (whole model, f64)
   MDL-COUNT-REGIONS                            \ tally region classes for the composed tolerance
   LOWER-MODEL-RUN                              \ device (fills LLA-HOUT = final node, f32 -> f64)
   MDL-ATOL MDL-RTOL LG-COMPARE-LIN
   if MDL-PASS-REASON V-PASS else MDL-FAIL-REASON V-FAIL then ;

end-package
