\ compile-floor.f - the compile-floor yardstick (tools/compile-floor.f), which
\ other lanes quote: its count line, a ratchet that refuses a floor of 0 and
\ passes an unmissable one, and the engine state it borrows given back. Ratchet
\ cases run real `bin/hb` children because the tool moves engine-global state;
\ the borrow cases run in this process, the only one that can see the borrow.
\ The report-only child is test/compile-floor-gate.f's floor case.

require src/lib/errors.f
require src/lib/string.f
require src/lib/test.f
require src/lib/memory.f
require src/lib/fs.f
require src/lib/process.f
require src/lib/process-argv.f
require src/lib/process-env.f
require src/lib/argv.f
require src/lib/test/outcome.f

package COMPILE-FLOOR-TEST

private

$1000 constant CAP
120000 constant TIMEOUT-MS         \ five rounds of three 100-definition sets per run

create OUT CAP allot
create ERR CAP allot
variable OUT-U  variable ERR-U
variable RC     variable EXITED

: OUT$ ( -- ptr u8 n )  OUT OUT-U @ ;
: ERR$ ( -- ptr u8 n )  ERR ERR-U @ ;

: TOOL$ ( -- ptr u8 n )  s" tools/compile-floor.f" ;

: STORE! ( len len outcome -- )
   {: outu:len erru:len oc :}
   outu LEN>N OUT-U !  erru LEN>N ERR-U !
   oc MATCH outcome
     exited   OF RC ! 0 0= EXITED ! ENDOF
     signaled OF RC ! 0 0= 0= EXITED ! ENDOF
     timeout  OF TOOL$ OUT$ ERR$ T-TIMED-OUT ENDOF
   ;MATCH ;

\ The binary under test, honouring the suite's override the same way test/tier.f
\ does, so a build driver can point every child at the engine it just built.
: HB$ ( -- ptr u8 n )
   s" HABU_UNDER_TEST" >LEN PROC-ENV-DEFAULT$? if LEN>N exit then
   2drop
   s" HABU_UNDER_TEST" GETENV dup 0= if
      2drop s" bin/hb" exit
   then ;

: ARG+ ( ptr u8 n -- )  >LEN PROC-ARGV+ ;

: RUN-FLOOR ( ptr u8 n -- ) {: arg:ptr au :}
   PROC-ARGV-RESET
   s" --load" ARG+
   TOOL$ ARG+
   s" --" ARG+  arg au ARG+
   HB$ >LEN  OUT CAP >LEN  ERR CAP >LEN  TIMEOUT-MS >MS
   RUN-ARGV-CAPTURE-OUTCOME STORE! ;

\ `compiled 1000` is the tool's count of NCOMP:COMPILE dispatches (five rounds
\ of two sets): a floor without it is a tier-0 number under a tier-1 label. It
\ is asserted on every child, refusing or not: the tool prints it before the
\ ratchet.
: ASSERT-COUNTED ( -- )
   EXITED @ TTRUE
   OUT$ s" floor: trivial-t1 " CONTAINS? TTRUE
   OUT$ s" three-op-t1 " CONTAINS? TTRUE
   HB-TARGET-LINUX-X86-64? if
      OUT$ s" trivial-t0 " CONTAINS? TFALSE
   else
      OUT$ s" trivial-t0 " CONTAINS? TTRUE
   then
   OUT$ s" compiled 1000 " CONTAINS? TTRUE ;

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
\ NCOMP-DISPATCH:XT-CELL and the tier: an unreturned borrow still exits 0 and
\ counts the caller's later definitions. ORIG-XT is captured before the tool
\ loads: requiring the tool runs it, so a later snapshot would compare its
\ wrapper with itself and pass (checked by deleting the restore). The tier is
\ snapshotted per control, each moving to tier 1 first: on ARM the tool measures
\ tier 0 last, so only a tier-1 caller tells "put back" from "happened to
\ match".

variable ORIG-XT
variable PRIOR-TIER
variable ENTRY-TIER

\ `=` has no row over quotation terms, so the accessor declares the cell as the
\ machine word it compares; the tool only stores through NCOMP:DISPATCH! and
\ ends the borrow with NCOMP:INSTALL.
: DISPATCH-WORD ( -- ptr n )  data-base NCOMP-DISPATCH:XT-CELL + ;
: DISPATCH-KEPT? ( -- bool )  DISPATCH-WORD @ ORIG-XT @ = ;
: TIER-KEPT? ( -- bool )  tier@ PRIOR-TIER @ = ;

DISPATCH-WORD @ ORIG-XT !
tier@ ENTRY-TIER !

\ Requiring the tool RUNS it: that run is the success control. Empty the mock
\ argv first so that load-time run is the reporting one whatever argv this
\ file itself was given.
1 set-tier
tier@ PRIOR-TIER !
ARGV:MOCK-CLEAR

;package

require tools/compile-floor.f

package COMPILE-FLOOR-TEST

private

: CATCH-MAIN ( -- n )  [: COMPILE-FLOOR:MAIN ;] catch ;

\ `tier@ 1 T=` pins that the control ran from tier 1, which gives TIER-KEPT? its
\ meaning.
: ASSERT-GAVE-BACK ( -- )
   DISPATCH-KEPT? TTRUE
   TIER-KEPT? TTRUE
   tier@ 1 T= ;

: TEST-RESTORES-AFTER-LOAD ( -- )
   s" the completed load-time run gives the dispatch and the tier back" T-LABEL
   ASSERT-GAVE-BACK ;

\ That the refusal case runs at all tests the teardown: it is this process's
\ second MAIN, and a MAIN that left its 300 measurement names behind fails to
\ reach an assertion, with `duplicate definition: T1`.
: TEST-RESTORES-AFTER-REFUSAL ( -- )
   s" a refused in-process run gives both back on the way out" T-LABEL
   1 set-tier
   tier@ PRIOR-TIER !
   ARGV:MOCK-CLEAR  s" 0" ARGV:MOCK+
   CATCH-MAIN E-FLOOR-EXCEEDED T=
   ASSERT-GAVE-BACK ;

public

: RUN-ALL ( -- )
   T-RESET
   TEST-RESTORES-AFTER-LOAD
   TEST-RATCHET-REFUSES
   TEST-RATCHET-PASSES
   TEST-RESTORES-AFTER-REFUSAL
   ENTRY-TIER @ set-tier
   ARGV:USE-SCRIPT
   T-REPORT
   s" compile-floor: ok" type cr ;

;package

COMPILE-FLOOR-TEST:RUN-ALL
