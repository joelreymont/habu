\ compile-floor.f - the compile-floor yardstick, asserted through real children
\ and through this process.
\
\ tools/compile-floor.f exists to be QUOTED by other lanes, so the things that
\ can silently spoil a quote are what this file pins:
\
\   1. The line is there and carries its proof. `compiled 200` is the tool's
\      own count of dispatches into NCOMP:COMPILE. A run that reported a floor
\      without it would be a tier-0 number wearing a tier-1 label, and the
\      whole yardstick would be off by two orders of magnitude.
\   2. The ratchet has both directions. A ratchet that only ever exits 0 is
\      not a ratchet, and one that only ever exits nonzero blocks the lanes it
\      is meant to measure. A floor of 0 must refuse and say why; a floor no
\      current engine could miss must pass.
\   3. The tool gives the engine back. It borrows NCOMP-DISPATCH:XT-CELL and
\      the tier, and a borrow that is not returned is invisible to every
\      exit-code check: the process still exits 0, and every definition the
\      caller compiles afterwards is counted by a tool that has finished
\      running. Cases 4 and 5 are the only ones that can see it.
\
\ Cases 1 to 3 are REAL `bin/hb` children, because the tool moves engine-global
\ state and a suite that ran them in-process would be measuring whatever the
\ rest of the suite left behind. Cases 4 and 5 must be the opposite - the
\ borrow is only observable from inside the process that lent it - so they call
\ COMPILE-FLOOR:MAIN here and read the cell and the tier back afterwards.
\
\ BOTH CONTROLS RUN FROM TIER 1, and that is the whole point of them. The tool
\ measures its tier-0 contrast last, so a MAIN that restored nothing at all
\ would still leave a tier-0 caller on tier 0 and pass a tier-0 control. Only a
\ caller that was on tier 1 can tell "put back" from "happened to match".

require lib/errors.f
require lib/string.f
require lib/test.f
require lib/memory.f
require lib/fs.f
require lib/process.f
require lib/process-argv.f
require lib/process-env.f
require lib/argv.f

package COMPILE-FLOOR-TEST

private

$1000 constant CAP
120000 constant TIMEOUT-MS         \ three measured sets per run, each about 100 definitions

create OUT CAP allot
create ERR CAP allot
variable OUT-U  variable ERR-U
variable RC     variable EXITED

: OUT$ ( -- ptr u8 n )  OUT OUT-U @ ;
: ERR$ ( -- ptr u8 n )  ERR ERR-U @ ;

: STORE! ( len len outcome -- )
   MATCH outcome
     exited   OF RC ! 0 0= EXITED ! ENDOF
     signaled OF RC ! 0 0= 0= EXITED ! ENDOF
     timeout  OF 0 RC ! 0 0= 0= EXITED ! ENDOF
   ;MATCH
   LEN>N ERR-U !  LEN>N OUT-U ! ;

\ The binary under test, honouring the suite's override the same way test/tier.f
\ does, so a build driver can point every child at the engine it just built.
: HB$ ( -- ptr u8 n )
   s" HABU_UNDER_TEST" >LEN PROC-ENV-DEFAULT$? if LEN>N exit then
   2drop
   s" HABU_UNDER_TEST" GETENV dup 0= if
      2drop s" bin/hb" exit
   then ;

: ARG+ ( ptr u8 n -- )  >LEN PROC-ARGV+ ;

\ An empty argument means "no floor": the reporting invocation.
: RUN-FLOOR ( ptr u8 n -- ) {: arg:ptr au :}
   PROC-ARGV-RESET
   s" --load" ARG+
   s" tools/compile-floor.f" ARG+
   au 0 > if s" --" ARG+  arg au ARG+ then
   HB$ >LEN  OUT CAP >LEN  ERR CAP >LEN  TIMEOUT-MS >MS
   RUN-ARGV-CAPTURE-OUTCOME STORE! ;

\ The count line is asserted on every child case, refusing or not: the tool
\ prints the measurement before it applies the ratchet, and a ratchet failure
\ that swallowed the number would leave the lanes with nothing to report.
: ASSERT-COUNTED ( -- )
   EXITED @ TTRUE
   OUT$ s" floor: trivial-t1 " CONTAINS? TTRUE
   OUT$ s" three-op-t1 " CONTAINS? TTRUE
   OUT$ s" trivial-t0 " CONTAINS? TTRUE
   OUT$ s" compiled 200" CONTAINS? TTRUE ;

: TEST-REPORTS ( -- )
   s" without a floor the tool reports and exits 0" T-LABEL
   s" " RUN-FLOOR
   ASSERT-COUNTED
   RC @ 0 T= ;

: TEST-RATCHET-REFUSES ( -- )
   s" a floor of 0 refuses, names the number, and exits nonzero" T-LABEL
   s" 0" RUN-FLOOR
   ASSERT-COUNTED
   RC @ 0 <> TTRUE
   ERR$ s" compile-floor: trivial-t1 " CONTAINS? TTRUE
   ERR$ s" is above the floor of 0 us" CONTAINS? TTRUE ;

: TEST-RATCHET-PASSES ( -- )
   s" a floor no current engine could miss exits 0" T-LABEL
   s" 1000" RUN-FLOOR
   ASSERT-COUNTED
   RC @ 0 T= ;

\ ---- the state the tool borrows from this process ---------------------------
\
\ ORIG-XT IS CAPTURED BEFORE THE TOOL IS EVER LOADED, and that is not fussiness.
\ Requiring the tool runs it, so a tool that restored nothing would already have
\ left its own wrapper in the cell by the time any later snapshot was taken -
\ and a control that compared against THAT would be comparing the wrapper with
\ itself and passing. Only a reference that predates the first run can tell the
\ engine's dispatch from the tool's. (Checked: with the restore deleted, a
\ snapshot taken after the load passes and this one fails.)
\
\ The tier is the opposite case and is snapshotted per control, because each
\ control deliberately moves to tier 1 first.

variable ORIG-XT
variable PRIOR-TIER

TRUSTED: SELECT-TIER ( n -- ) set-tier ;

: DISPATCH-CELL ( -- ptr a )  data-base NCOMP-DISPATCH:XT-CELL + ;
: DISPATCH-KEPT? ( -- bool )  DISPATCH-CELL @ ORIG-XT @ = ;
: TIER-KEPT? ( -- bool )  tier@ PRIOR-TIER @ = ;

DISPATCH-CELL @ ORIG-XT !

\ Requiring the tool RUNS it. Empty the mock argv first so that load-time run
\ is the reporting one whatever argv this file itself was given.
ARGV:MOCK-CLEAR

;package

require tools/compile-floor.f

package COMPILE-FLOOR-TEST

private

: CATCH-MAIN ( -- n )  [: COMPILE-FLOOR:MAIN ;] catch ;

\ `tier@ 1 T=` is not a restatement of TIER-KEPT?: it pins that the control
\ really did run from tier 1, which is the only thing that makes TIER-KEPT?
\ mean anything here. Delete it and a tier-0 control passes for free.
: ASSERT-GAVE-BACK ( -- )
   DISPATCH-KEPT? TTRUE
   TIER-KEPT? TTRUE
   tier@ 1 T= ;

\ That these two cases run AT ALL is also the teardown's test. They are the
\ second and third MAIN of this process (the load-time run was the first), and
\ the engine refuses a redefinition outright - so a MAIN that left its 300
\ measurement names behind would not fail an assertion here, it would fail to
\ reach one, with `duplicate definition: T1`.
: TEST-RESTORES-AFTER-SUCCESS ( -- )
   s" a completed in-process run gives the dispatch and the tier back" T-LABEL
   1 SELECT-TIER
   tier@ PRIOR-TIER !
   ARGV:MOCK-CLEAR
   CATCH-MAIN 0 T=
   ASSERT-GAVE-BACK ;

: TEST-RESTORES-AFTER-REFUSAL ( -- )
   s" a refused in-process run gives both back on the way out" T-LABEL
   1 SELECT-TIER
   tier@ PRIOR-TIER !
   ARGV:MOCK-CLEAR  s" 0" ARGV:MOCK+
   CATCH-MAIN E-FLOOR-EXCEEDED T=
   ASSERT-GAVE-BACK ;

public

: RUN-ALL ( -- )
   T-RESET
   TEST-REPORTS
   TEST-RATCHET-REFUSES
   TEST-RATCHET-PASSES
   TEST-RESTORES-AFTER-SUCCESS
   TEST-RESTORES-AFTER-REFUSAL
   0 SELECT-TIER
   ARGV:USE-SCRIPT
   T-REPORT
   s" compile-floor: ok" type cr ;

;package

COMPILE-FLOOR-TEST:RUN-ALL
