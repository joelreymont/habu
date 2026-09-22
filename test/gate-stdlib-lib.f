\ gate-stdlib-lib.f - adapter from the native suite registry to bin/hb.

require lib/test.f
require lib/test/runner.f
require test/gate-pool.f
require test/cold-engine.f
require test/whitebox-engine.f
require lib/engine-id.f                  \ ENGINE-ID:PATH$ - this gate's own binary

package STDLIB-GATE

360000 constant SUITE-TIMEOUT-MS
64 constant SUITE-USAGE-RC

create WB-BUF FS-PATH-CAP allot
variable WB-U

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

: SUITE-ENV ( ptr u8 n -- ) {: eng:ptr engu:n :}
   PROC-ENV-RESET
   eng engu SUITE-ENGINE-ENV
   PROC-ENV-INHERIT-MISSING ;

: SUITE-RUN-ASYNC ( ptr u8 n n ptr u8 n -- ) {: path:ptr pathu:n timeout:n label:ptr labelu:n :}
   path pathu label labelu timeout GT-POOL-START ;

\ Every argv entry goes through as written. The runner used to prepend
\ test/compiler/aot-mode.f - `1 set-tier` - to every `test/compiler/native-*.f`
\ row, so a suite whose assertions are tier-1 facts was green here and red on
\ its own. The tier belongs to the code under test: such a file selects it
\ before its requires, and every row now measures what `bin/hb --load <file>`
\ measures.
: SUITE-HB ( -- )
   PROC-ARGV-RESET
   s" --load" >LEN PROC-ARGV+ ;

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

: SUITE-HB-RUN ( ptr u8 n -- ) {: label:ptr labelu:n :}
   PRODUCT-ENGINE$ SUITE-ENV
   PRODUCT-SPAWN$ SUITE-TIMEOUT-MS label labelu SUITE-RUN-ASYNC ;

: WB-PATH$ ( -- ptr u8 n )
   WB-BUF WB-U @ ;

\ The whitebox engine, in the gate's own temp root: a private copy per gate run,
\ so a suite that spawns it cannot reach - or overwrite - the shared keyed
\ artifact, and the tree's bin/hb keeps its seal.
: WB-ENSURE ( -- )
   0 WB-U !
   TEST:WHITEBOX-REGISTERED? 0= if exit then
   s" hb-whitebox" WB-BUF GT-PATH WB-U !
   WB-PATH$ WHITEBOX-ENGINE:PROVIDE ;

: SUITE-WB-RUN ( ptr u8 n -- ) {: label:ptr labelu:n :}
   WB-U @ 0 <= if E-FS-PATH throw then
   WB-PATH$ SUITE-ENV
   WB-PATH$ SUITE-TIMEOUT-MS label labelu SUITE-RUN-ASYNC ;

: SUITE-HB-RUN-STDIN ( ptr u8 n ptr u8 n -- ) {: in:ptr inu:n label:ptr labelu:n :}
   PRODUCT-ENGINE$ SUITE-ENV
   PRODUCT-SPAWN$ label labelu in inu SUITE-TIMEOUT-MS GT-POOL-START-STDIN ;

\ Emit the shared cold fixture host here, before the first suite forks: the
\ fixtures that need it then copy one keyed artifact instead of each paying the
\ writer's own native build.
: SUITE-SETUP ( -- )
   SUITE-CHECK-ARGS
   s" habu-native-suite" GT-START
   COLD-ENGINE:ENSURE
   WB-ENSURE
   GT-POOL-RESET ;

\ The pool drains softly between groups, so every registered suite runs
\ whatever went red before it; the complete red set is reported here, once,
\ with each suite's exit code, and decides the exit status.
: SUITE-FINISH ( n -- ) {: rc:n :}
   s" suites: ran " type TEST:ITEMS-RUN GT-U-TYPE
   s"  of " type TEST:ITEMS-REGISTERED GT-U-TYPE cr
   GT-POOL-RED-REPORT
   rc 0 <> if exit then                 \ the body threw: the framework rethrows that code
   GT-POOL-RED# 0 > if s" test pool failed" 1 die then
   GT-CLEANUP ;

: SUITE-INSTALL-HOOKS ( -- )
   [: SUITE-SETUP ;] TEST:SETUP!
   [: SUITE-FINISH ;] TEST:TEARDOWN!
   [: GT-POOL-DRAIN-SOFT ;] TEST:DRAIN!
   [: SUITE-HB ;] TEST:ARGS-BEGIN!
   [: >LEN PROC-ARGV+ ;] TEST:ARG+!
   [: SUITE-HB-RUN ;] TEST:RUNNER!
   [: SUITE-WB-RUN ;] TEST:WHITEBOX-RUNNER!
   [: SUITE-HB-RUN-STDIN ;] TEST:STDIN-RUNNER! ;

public

: MAIN ( -- )
   SUITE-INSTALL-HOOKS
   TEST:RESET ;

;package
