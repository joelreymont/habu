\ gate-stdlib-lib.f - adapter from the native suite registry to bin/hb.

require lib/test.f
require lib/test/runner.f
require lib/process-argv.f
require test/gate-pool.f
require test/gate-images.f
require test/image-grant.f
require test/gate-entry-guard.f
require test/suite-budget.f              \ a row's CPU budget and hang guard
require lib/engine-id.f                  \ ENGINE-ID:PATH$ - this gate's own binary

package STDLIB-GATE

64 constant SUITE-USAGE-RC
\ A row's argv tokens, each a length cell and its bytes: at most what the
\ process-wide argv table takes.
PROC-ARGV-BUF-CAP PROC-ARGV-MAX cells + constant ROW-ARGS-CAP

create ROW-ARGS ROW-ARGS-CAP allot
variable ROW-ARGS-U
variable ROW-ARG-N
variable ROW-SCAN
variable ROW-SCRIPT?                     \ the row's `--` has passed

: SUITE-USAGE ( -- )
   s" usage: bin/hb --load test/run.f" SUITE-USAGE-RC die ;

: SUITE-CHECK-ARGS ( -- )
   ARGC 3 > if 3 SCRIPT-SEP? 0= if SUITE-USAGE then then
   SCRIPT-ARGC 0 <> if SUITE-USAGE then ;

\ EVERY ITEM NAMES ITS OWN ENGINE TO ITS CHILDREN. A suite forks tools of its
\ own, and lib/engine-candidate.f is the one resolver they all ask which engine
\ to run: it reads HABU_UNDER_TEST first and falls back to the running engine
\ only when nothing names one. The gate used to pass its environment through
\ untouched, so a CI that exports HABU_UNDER_TEST=<tree>/bin/hb - which is the
\ ordinary way to point a gate at a candidate - sent the children of a WHITEBOX
\ item to the sealed product while the item itself ran on the whitebox engine.
\ Thirteen suites went red with their CHILD answering `hb: internal engine word:
\ DECLARATIONS`, and the item's own engine was never the question.
\
\ So the item states the answer instead of inheriting it, for both kinds: the
\ variable a child resolves names the binary this item is being run on, and an
\ outer value cannot reach past it. HABU_FIXPOINT_ENGINE goes with it because a
\ suite that rebuilds an engine (tools/build-fixpoint.f BF-ENGINE$) must land on
\ the same one - and for a whitebox item that is the gate's own private copy,
\ which is exactly where a build that promotes over it should write.
\
\ Set BEFORE the inherit, which skips a name already present, so each variable
\ has one row whatever the parent's environment holds.
: SUITE-ENGINE-ENV ( ptr u8 n -- ) {: eng:ptr engu:n :}
   s" HABU_UNDER_TEST" >LEN eng engu >LEN PROC-ENV+
   s" HABU_FIXPOINT_ENGINE" >LEN eng engu >LEN PROC-ENV+ ;

\ The grant names the keyed images the gate settled for this row
\ (test/image-grant.f), and goes in before the inherit for the same reason: a
\ gate run as a row of another gate grants its own rows, never its parent's.
: SUITE-ENV ( ptr u8 n ptr u8 n -- ) {: eng:ptr engu:n grant:ptr grantu:n :}
   PROC-ENV-RESET
   eng engu SUITE-ENGINE-ENV
   IMAGE-GRANT:NAME$ >LEN grant grantu >LEN PROC-ENV+
   PROC-ENV-INHERIT-MISSING ;

\ The product engine, spelled twice for two different readers. The SPAWN is
\ relative, the way every other spawn of it in the tree is: the gate already has
\ to be running in the checkout. The VARIABLE is the same binary spelled
\ absolutely, because the child that resolves it may run somewhere else -
\ test/boot-row-test.f spawns the engine under a caller's own CWD, and a
\ relative path does not survive that (measured: E-PROC-SPAWN, exit 67).
\ ENGINE-ID:PATH$ is this process's own executable, so the two always name one
\ file.
: PRODUCT-SPAWN$ ( -- ptr u8 n )
   s" bin/hb" ;

: PRODUCT-ENGINE$ ( -- ptr u8 n )
   ENGINE-ID:PATH$ ;

\ A ROW'S ARGV WAITS WITH IT. The row's files name the keyed images it needs
\ (test/gate-images.f), and a row whose images are not settled yet holds the
\ registry while build rows start - each staging its own argv in the same
\ process-wide table. So the tokens are kept here as they arrive and staged
\ only once the row is ready to spawn.
: ROW-BEGIN ( -- )
   0 ROW-ARGS-U !
   0 ROW-ARG-N !
   0 0<> ROW-SCRIPT? !
   GATE-IMAGES:ROW-RESET ;

\ Every argv entry goes through as written. The runner used to prepend
\ test/compiler/aot-mode.f - `1 set-tier` - to every `test/compiler/native-*.f`
\ row, so a suite whose assertions are tier-1 facts was green here and red on
\ its own. The tier belongs to the code under test: such a file selects it
\ after harness and tool requires but before its subject definitions. Every
\ row measures what `bin/hb --load <file>` measures.
: ROW-ARG+ ( ptr u8 n -- ) {: a:ptr u:n :}
   ROW-ARGS-U @ u + cell + ROW-ARGS-CAP > if E-STR-CAPACITY throw then
   u ROW-ARGS ROW-ARGS-U @ + !
   a ROW-ARGS ROW-ARGS-U @ + cell + u BYTE-COPY
   ROW-ARGS-U @ cell + u + ROW-ARGS-U !
   ROW-ARG-N @ 1+ ROW-ARG-N !
   a u s" --" STR= if 0 0= ROW-SCRIPT? ! exit then
   ROW-SCRIPT? @ 0= if a u GATE-IMAGES:ROW-FILE then ;

: ROW-ARGV! ( -- )
   PROC-ARGV-RESET
   s" --load" >LEN PROC-ARGV+
   0 ROW-SCAN !
   ROW-ARG-N @ 0 ?do
      ROW-ARGS ROW-SCAN @ + @ {: u:n :}
      ROW-ARGS ROW-SCAN @ + cell + u >LEN PROC-ARGV+
      ROW-SCAN @ cell + u + ROW-SCAN !
   loop ;

\ TRUE once every keyed image the row needs is settled, with its argv staged;
\ else the row is started as its own red row naming the failed build row.
: SUITE-READY? ( ptr u8 n -- bool ) {: label:ptr labelu:n :}
   GATE-IMAGES:ROW-READY? if ROW-ARGV! 0 0= exit then
   label labelu GATE-IMAGES:ROW-RED
   0 0<> ;

\ A suite row is held to a CPU budget, whatever the load (test/suite-budget.f
\ BOUNDED BY ITS OWN WORK): each start below gives the row the hang guard
\ SUITE-BUDGET:ROW-MS as its deadline, and ROW-BUDGET! the budget.
: ROW-BUDGET! ( -- )
   SUITE-BUDGET:CPU-MS GT-POOL-SEQ @ GT-POOL-CPU-BUDGET! ;

: SUITE-HB-RUN ( ptr u8 n -- ) {: label:ptr labelu:n :}
   label labelu SUITE-READY? 0= if exit then
   PRODUCT-ENGINE$ GATE-IMAGES:ROW-GRANT$ SUITE-ENV
   PRODUCT-SPAWN$ label labelu SUITE-BUDGET:ROW-MS GT-POOL-START
   ROW-BUDGET! ;

\ A WHITEBOX-SUITE row runs on the gate's private copy of the unsealed engine,
\ which the whitebox-engine build row puts in place.
: SUITE-WB-RUN ( ptr u8 n -- ) {: label:ptr labelu:n :}
   GATE-IMAGES:ROW-WHITEBOX
   label labelu SUITE-READY? 0= if exit then
   GATE-IMAGES:WHITEBOX$ GATE-IMAGES:ROW-GRANT$ SUITE-ENV
   GATE-IMAGES:WHITEBOX$ label labelu SUITE-BUDGET:ROW-MS GT-POOL-START
   ROW-BUDGET! ;

: SUITE-HB-RUN-STDIN ( ptr u8 n ptr u8 n -- ) {: in:ptr inu:n label:ptr labelu:n :}
   label labelu SUITE-READY? 0= if exit then
   PRODUCT-ENGINE$ GATE-IMAGES:ROW-GRANT$ SUITE-ENV
   PRODUCT-SPAWN$ label labelu in inu SUITE-BUDGET:ROW-MS GT-POOL-START-STDIN
   ROW-BUDGET! ;

\ A keyed image's build row: the product engine, handed its program on stdin
\ with no arguments, granted the family it settles and that family's
\ prerequisites.
: SUITE-BUILD-RUN ( ptr u8 n ptr u8 n ptr u8 n n -- )
   {: prog:ptr progu:n label:ptr labelu:n grant:ptr grantu:n timeout:n :}
   PROC-ARGV-RESET
   PRODUCT-ENGINE$ grant grantu SUITE-ENV
   PRODUCT-SPAWN$ label labelu prog progu timeout GT-POOL-START-STDIN ;

\ The entry guard walks the load graph DERIVE read and refuses the registry
\ before anything starts: this hook runs outside the catch around the rows. The
\ keyed images the registry's rows load - the fixture writer, the cold host,
\ the saver and linker images, the unsealed engine - are settled in the pool,
\ one build row per image beside the rows, and each row starts once its own
\ images are (test/gate-images.f): a broken image closure fails its build row
\ and the rows that load it, and every other row runs.
: SUITE-SETUP ( -- )
   SUITE-CHECK-ARGS
   GATE-IMAGES:DERIVE
   ENTRY-GUARD:CHECK
   GT-POOL-CATCH-SIGNALS
   s" habu-native-suite" GT-START
   GT-POOL-RESET
   [: SUITE-BUILD-RUN ;] GATE-IMAGES:START ;

\ A sequential group runs alone: every build row has retired before the pool
\ drains for it.
: SUITE-DRAIN ( -- )
   GATE-IMAGES:SETTLE-ALL
   GT-POOL-DRAIN-SOFT ;

\ The pool drains softly between groups, so every registered suite runs
\ whatever went red before it; the complete red set is reported here, once,
\ with each suite's exit code, and decides the exit status.
\
\ The report is complete before the tree goes: every red row's output and its
\ capture-file names are already on stdout, so the cleanup takes nothing the
\ reader still needs. It runs BEFORE the red die, which ends the process - a
\ red gate used to leave its whole pool root, and a run per red is how /tmp
\ filled with habu-native-suite trees.
: SUITE-FINISH ( n -- ) {: rc:n :}
   s" suites: ran " type TEST:ITEMS-RUN GT-U-TYPE
   s"  of " type TEST:ITEMS-REGISTERED GT-U-TYPE cr
   GT-POOL-RED-REPORT
   GT-CLEANUP
   GT-POOL-SIGNAL-CHECK
   rc 0 <> if exit then                 \ the body threw: the framework rethrows that code
   GT-POOL-RED# 0 > if s" test pool failed" 1 die then ;

: SUITE-INSTALL-HOOKS ( -- )
   [: SUITE-SETUP ;] TEST:SETUP!
   [: SUITE-FINISH ;] TEST:TEARDOWN!
   [: SUITE-DRAIN ;] TEST:DRAIN!
   [: ROW-BEGIN ;] TEST:ARGS-BEGIN!
   [: ROW-ARG+ ;] TEST:ARG+!
   [: SUITE-HB-RUN ;] TEST:RUNNER!
   [: SUITE-WB-RUN ;] TEST:WHITEBOX-RUNNER!
   [: SUITE-HB-RUN-STDIN ;] TEST:STDIN-RUNNER! ;

public

: MAIN ( -- )
   SUITE-INSTALL-HOOKS
   TEST:RESET ;

;package
