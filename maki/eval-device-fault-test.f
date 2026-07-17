\ maki/eval-device-fault-test.f - regression: a launch FAULT is graded, not fatal.
\
\ Guards the grader's stated contract (maki/eval-device.f: "candidate is a graded
\ failure, never a grader casualty") against the device-launch path. A ptxas-clean
\ but type-buggy no-check candidate -- a raw span pointer used as the grid index --
\ does an out-of-bounds GPU read -> contained nvgpu MMU fault -> nonzero CUresult.
\ Before the fork-isolation fix that throw (E-CUDA) killed the grader before any
\ tally printed; now GRADE-NOCHECK-CANDIDATE runs each launch in its own forked
\ child and grades the fault as the distinct EVN-DEVICE-FAULT bucket. The
\ regression proves BOTH: the faulter grades as a fault AND the grader survives it
\ (the very next candidate still grades GREEN).
\
\ Device-gated (maki/device-smoke.f pattern): off the Orin (no libcuda) it is a
\ recorded SKIP and this file check-loads; on the Orin it runs the real launches.
\ Reopens package EVAL to read the private EVN-* buckets and call the grader bare.

require lib/test.f
require maki/eval-device.f

package EVAL

\ The span-as-gridctx candidate from the ablation fixture (maki/eval-compare.f):
\ x (a span) is used as BOTH the base AND the element index -> out-of-bounds read.
\ The checker REJECTS it, so the no-check arm is the only path that reaches device.
: EDFT-FAULTER$ ( -- ptr u8 n )
   s" K ( span<space-global,f32,extent-n> span<space-global,f32,extent-n> uniform<f32> -- ) {: x y a :} x x LOAD a SCALE y x LOAD +. y x STORE" ;

\ A correct SAXPY phrasing: a*x+y = 6.0 -> device-correct -> EVN-GREEN.
: EDFT-CORRECT$ ( -- ptr u8 n )
   s" K ( span<space-global,f32,extent-n> span<space-global,f32,extent-n> uniform<f32> -- ) {: x y a :} x GRID-CTX {: g :} x g LOAD a SCALE y g LOAD +. y g STORE" ;

: EDFT-RUN ( -- )
   T-RESET
   CUDA:OPEN? 0= if
      s" eval-device-fault: libcuda unavailable -> launch-fault regression SKIPPED (off-device; file check-loads)" type cr
      T-REPORT exit then
   \ a ptxas-clean-but-launch-faulting candidate GRADES as a fault, not a crash
   EDFT-FAULTER$ GRADE-NOCHECK-CANDIDATE  EVN-DEVICE-FAULT T=
   \ ...and the grader survives it: the very next candidate still grades GREEN
   EDFT-CORRECT$ GRADE-NOCHECK-CANDIDATE  EVN-GREEN T=
   s" eval-device-fault: launch fault graded and the grader continued" type cr
   T-REPORT ;

EDFT-RUN

;package
