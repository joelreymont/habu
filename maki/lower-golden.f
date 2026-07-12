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
\ Tolerance policy: the per-class rows are OWNED BY maki/precision.f (CAD-PLAN
\ section 11 + 8.1 lever 5). Each region class carries an ACTIVE precision
\ (default PREC-F32; PREC! requests a demotion such as PREC-TF32 for the matmul
\ class) and the golden judges under that precision's (atol, rtol) row - the
\ passing verdict IS the license for running that class at that precision. The
\ verdict reason names the judged precision ("within tf32 tol") and
\ LG-PREC-USED@ exposes it for the PROMOTE evidence row.
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
require maki/precision.f
require maki/model-ir.f
require maki/fusion-plan.f
require maki/executor.f
require maki/golden-artifact.f
require maki/lower-launch.f

package MAKI

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

\ ---- judged precision (the verdict's licensed-precision fact) ----------------
variable LG-PREC-V                              \ precision id the last verdict was judged under
: LG-PREC$ ( -- ptr u8 n )  LG-PREC-V @ PREC-NAME ;
public
: LG-PREC-USED@ ( -- n )  LG-PREC-V @ ;
private

\ ---- tolerance comparison (device f32 vs host rounded to the f32 grid) -------
: LG-NARROW ( r -- r )  F64>F32 F32>F64 ;      \ round a host f64 onto the f32 grid

variable LG-BADI                                \ first mismatched element index (-1 = none)
: LG-WITHIN-LIN? ( r r r r -- bool ) {: dev:r host:r atol:r rtol:r :}
   atol  rtol host fabs f*  f+ {: tol:r :}
   dev host f- fabs {: d:r :}
   tol d f< 0= ;                                \ pass iff |dev-host| <= tol
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

\ RGN>RAW is the one verdict-reason render boundary (REGION_<rid>)
: LG-PASS-REASON ( CAD-KIND:region -- ) {: rid:CAD-KIND:region :}
   LG-RE-RESET
   s" lower-golden: REGION_" LG-RE+ rid RGN>RAW LG-RE-INT
   s"  device==host within " LG-RE+ LG-PREC$ LG-RE+
   s"  tol (" LG-RE+ LLA-ELEMS@ LG-RE-INT s"  elems)" LG-RE+ ;
: LG-FAIL-REASON ( CAD-KIND:region -- ) {: rid:CAD-KIND:region :}
   LG-RE-RESET
   s" lower-golden: REGION_" LG-RE+ rid RGN>RAW LG-RE-INT
   s"  mismatch beyond " LG-RE+ LG-PREC$ LG-RE+
   s"  tol at elem " LG-RE+ LG-BADI @ LG-RE-INT ;

\ region class -> launch shape + tolerance. A matmul bit routes to the tiled-GEMM kernel;
\ a reduction bit routes to the row kernel; otherwise the flat elementwise kernel. A region
\ never mixes a contraction with a reduction (maki/fusion-plan.f), so the order is disjoint.
\ region class routing is owned by lower-launch (LOWER-MODEL-RUN reuses it); alias here.
: LG-MATMUL? ( CAD-KIND:region -- bool )  LLA-REGION-MATMUL? ;
: LG-REDUCE? ( CAD-KIND:region -- bool )  LLA-REGION-REDUCE? ;

\ a MATERIALIZED movement region (the region's output node is a movement copy, not a fold)
\ routes to the copy-kernel launch; a dissolved-fold region keeps its EW/RED/MM class route.
: LG-MOVE? ( CAD-KIND:region -- bool )  LLA-REGION-MOVE? ;

\ region -> op-registry class id (the tolerance row + precision axis)
: LG-CLASS ( CAD-KIND:region -- n ) {: rid:CAD-KIND:region :}
   rid LG-MOVE?   if CLASS-MOVEMENT   exit then
   rid LG-MATMUL? if CLASS-MATMUL     exit then
   rid LG-REDUCE? if CLASS-ROW-REDUCE exit then
   CLASS-EW ;

: LG-RUN ( CAD-KIND:region n -- ) {: rid:CAD-KIND:region cls:n :}   \ launch the region on its class route
   cls CLASS-MOVEMENT   = if rid LMV-RUN  exit then
   cls CLASS-MATMUL     = if rid LMM-RUN  exit then
   cls CLASS-ROW-REDUCE = if rid LRED-RUN exit then
   rid LLA-RUN ;

public
\ LOWER-GOLDEN ( rid -- verdict ). Requires the region's cubin already assembled and its
\ path set via LLA-CUBIN! (the device tool does emit+ptxas first). The tolerance is the
\ region class's ACTIVE precision row (maki/precision.f); the verdict names it.
: LOWER-GOLDEN ( CAD-KIND:region -- n ) {: rid:CAD-KIND:region :}
   PREC-F32 LG-PREC-V !
   CUDA:OPEN? 0= if
      LG-RE-RESET s" lower-golden: off-device (libcuda unavailable)" LG-RE+  V-NOTRUN exit then
   GA-BIND-SYNTH                                \ bind + fill synthetic inputs (host + device share them)
   MIR-N@ EX-RUN-N                              \ host reference
   rid LG-CLASS {: cls:n :}
   cls PREC@ LG-PREC-V !                        \ the precision this verdict is judged under
   rid cls LG-RUN
   cls PREC-ATOL cls PREC-RTOL LG-COMPARE-LIN
   if rid LG-PASS-REASON V-PASS else rid LG-FAIL-REASON V-FAIL then ;

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
\ understate a deep chain). Each class contributes its ACTIVE precision row
\ (maki/precision.f), so a licensed tf32 matmul class widens only its own terms.
: MDL-ATOL ( -- r )
   MDL-N-EW@  s>f CLASS-EW         PREC-ATOL f*
   MDL-N-RED@ s>f CLASS-ROW-REDUCE PREC-ATOL f* f+
   MDL-N-MM@  s>f CLASS-MATMUL     PREC-ATOL f* f+
   MDL-N-MV@  s>f CLASS-MOVEMENT   PREC-ATOL f* f+ ;
: MDL-RTOL ( -- r )
   MDL-N-EW@  s>f CLASS-EW         PREC-RTOL f*
   MDL-N-RED@ s>f CLASS-ROW-REDUCE PREC-RTOL f* f+
   MDL-N-MM@  s>f CLASS-MATMUL     PREC-RTOL f* f+
   MDL-N-MV@  s>f CLASS-MOVEMENT   PREC-RTOL f* f+ ;

\ the model verdict's precision = the strongest demotion among the PRESENT classes
\ (v1: only the matmul class can be non-default, so this names the tf32 license).
: MDL-PREC ( -- n )
   PREC-F32
   MDL-N-EW@  0 > if CLASS-EW         PREC@ PREC-MAX then
   MDL-N-RED@ 0 > if CLASS-ROW-REDUCE PREC@ PREC-MAX then
   MDL-N-MM@  0 > if CLASS-MATMUL     PREC@ PREC-MAX then
   MDL-N-MV@  0 > if CLASS-MOVEMENT   PREC@ PREC-MAX then ;

: MDL-PASS-REASON ( -- )
   LG-RE-RESET
   s" lower-model-golden: device==host within composed " LG-RE+ LG-PREC$ LG-RE+
   s"  tol (" LG-RE+ LLA-ELEMS@ LG-RE-INT
   s"  elems, " LG-RE+ MDL-N-REGIONS@ LG-RE-INT s"  regions)" LG-RE+ ;
: MDL-FAIL-REASON ( -- )
   LG-RE-RESET
   s" lower-model-golden: mismatch beyond composed " LG-RE+ LG-PREC$ LG-RE+
   s"  tol at elem " LG-RE+ LG-BADI @ LG-RE-INT ;

public
\ LOWER-MODEL-GOLDEN ( -- verdict ). Requires FP-BUILD + each region's cubin registered
\ (MDL-CUBIN!). Off-device -> V-NOTRUN (reason in LOWER-GOLDEN-REASON$).
: LOWER-MODEL-GOLDEN ( -- n )
   PREC-F32 LG-PREC-V !
   CUDA:OPEN? 0= if
      LG-RE-RESET s" lower-model-golden: off-device (libcuda unavailable)" LG-RE+  V-NOTRUN exit then
   GA-BIND-SYNTH                                \ bind + fill synthetic inputs (host + device share them)
   MIR-N@ EX-RUN-N                              \ host reference (whole model, f64)
   MDL-COUNT-REGIONS                            \ tally region classes for the composed tolerance
   MDL-PREC LG-PREC-V !                         \ the composed verdict's judged precision
   LOWER-MODEL-RUN                              \ device (fills LLA-HOUT = final node, f32 -> f64)
   MDL-ATOL MDL-RTOL LG-COMPARE-LIN
   if MDL-PASS-REASON V-PASS else MDL-FAIL-REASON V-FAIL then ;

;package
