\ maki/eval/device-fault-test.f - regression: a launch FAULT is graded, not fatal.
\
\ Guards the grader's stated contract (maki/eval/device.f: "candidate is a graded
\ failure, never a grader casualty") against the device-launch path. A ptxas-clean
\ but type-buggy no-check candidate -- a raw span pointer used as the grid index --
\ does an out-of-bounds GPU read -> contained nvgpu MMU fault -> nonzero CUresult.
\ Before the isolation fix that throw (E-CUDA) killed the grader before any tally
\ printed; now GRADE-NOCHECK-CANDIDATE runs each launch in its own SPAWNED fresh
\ bin/hb child and grades the fault as the distinct EVN-DEVICE-FAULT bucket. The
\ spawn (not bare fork) also makes this suite safe INSIDE the maki gate, where an
\ earlier suite has already initialized CUDA in-process (CUDA is fork-unsafe). The
\ regression proves BOTH: the faulter grades as a fault AND the grader survives it
\ (the very next candidate still grades GREEN).
\
\ MACHINE-TELEMETRY CONTRACT (RCA dot habu-rca-xid-31-69a64702, 2026-07-21). The
\ deliberate fault is REAL: the driver logs one kernel-journal line per run --
\   NVRM: Xid 31 ... name=hb ... MMU Fault: ENGINE GRAPHICS GPC0 GPCCLIENT_T1_0
\   faulted @ <VA> ... FAULT_PTE ACCESS_TYPE_VIRT_READ
\ with a FRESH pid each time (the spawned child) and a per-boot-CONSTANT fault VA
\ (the child's allocation sequence is identical every run, so the bad address --
\ the span base reused as the element index -- lands at the same spot; the base
\ moves only across boots). This is EXPECTED and CONTAINED; it cost a day of
\ machine-health investigation when it was unannounced, so EDFT-ANNOUNCE now
\ writes a correlation line to the SYSTEM JOURNAL (logger -t habu-device-fault)
\ immediately before the faulting launch. Operators chasing an Xid 31 from an hb
\ pid: `journalctl -t habu-device-fault -S <time>` shows the annotation seconds
\ before the NVRM line. A real fault is load-bearing here -- the async
\ fault-surfaces-at-sync containment path cannot be proven by a mocked CUresult
\ -- so the fault is announced, never simulated away.
\
\ Device-gated (maki/device-smoke.f pattern): off the Orin (no libcuda) it is a
\ recorded SKIP and this file check-loads; on the Orin it runs the real launches.
\ Reopens package EVAL to read the private EVN-* buckets and call the grader bare.

require lib/test.f
require lib/process.f
require lib/process-argv.f
require maki/eval/device.f

package EVAL

\ The span-as-gridctx candidate from the ablation fixture (maki/eval/compare.f):
\ x (a span) is used as BOTH the base AND the element index -> out-of-bounds read.
\ The checker REJECTS it, so the no-check arm is the only path that reaches device.
: EDFT-FAULTER$ ( -- ptr u8 n )
   s" K ( span<space-global,f32,extent-n> span<space-global,f32,extent-n> uniform<f32> -- ) {: x y a :} x x LOAD a SCALE y x LOAD +. y x STORE" ;

\ A correct SAXPY phrasing: a*x+y = 6.0 -> device-correct -> EVN-GREEN.
: EDFT-CORRECT$ ( -- ptr u8 n )
   s" K ( span<space-global,f32,extent-n> span<space-global,f32,extent-n> uniform<f32> -- ) {: x y a :} x GRID-CTX {: g :} x g LOAD a SCALE y g LOAD +. y g STORE" ;

\ Announce the imminent deliberate fault to the system journal so the kernel's
\ NVRM Xid 31 line seconds later is attributable without an RCA (the telemetry
\ contract above). logger failing IS a test failure: an unannounced deliberate
\ fault is exactly the defect this word exists to prevent.
: EDFT-ANNOUNCE ( -- )
   PROC-ARGV-RESET
   s" -t" >LEN PROC-ARGV+
   s" habu-device-fault" >LEN PROC-ARGV+
   s" expected Xid 31 follows: deliberate contained GPU MMU-fault regression (maki/eval/device-fault-test.f) proving grader fault-isolation; faulting pid = a spawned fresh bin/hb child" >LEN PROC-ARGV+
   s" /usr/bin/logger" >LEN -1 >FD -1 >FD -1 >FD PROC-RUN-ARGV-IO-RC
   MATCH result ok OF drop ENDOF err OF drop E-PROC-SPAWN throw ENDOF ;MATCH ;

: EDFT-RUN ( -- )
   T-RESET
   CUDA:OPEN? 0= if
      s" eval-device-fault: libcuda unavailable -> launch-fault regression SKIPPED (off-device; file check-loads)" type cr
      T-REPORT exit then
   \ a ptxas-clean-but-launch-faulting candidate GRADES as a fault, not a crash
   EDFT-ANNOUNCE
   EDFT-FAULTER$ GRADE-NOCHECK-CANDIDATE  EVN-DEVICE-FAULT T=
   \ ...and the grader survives it: the very next candidate still grades GREEN
   EDFT-CORRECT$ GRADE-NOCHECK-CANDIDATE  EVN-GREEN T=
   s" eval-device-fault: launch fault graded and the grader continued" type cr
   T-REPORT ;

EDFT-RUN

;package
