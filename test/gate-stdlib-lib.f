\ gate-stdlib-lib.f - adapter from the native suite registry to bin/hb.

require src/lib/string.f
require src/lib/test.f
require src/lib/test/runner.f
require src/lib/process-argv.f
require src/lib/process-env.f
require src/lib/fs.f
require test/gate-pool.f
require test/gate-images.f
require test/image-grant.f
require test/gate-entry-guard.f
require src/lib/engine-id.f                  \ ENGINE-ID:PATH$ - this gate's own binary

package STDLIB-GATE

64 constant SUITE-USAGE-RC
\ A row's argv tokens: their bytes end to end, and each one's length.
DYNAMIC-BUFFER ROW-ARG-BYTES u8
DYNAMIC-BUFFER ROW-ARG-LENS n
variable ROW-ARGS-U
variable ROW-ARG-N
variable ROW-SCAN
variable ROW-SCRIPT?                     \ the row's `--` has passed

: SUITE-USAGE ( -- )
   s" usage: bin/hb --load test/run.f" SUITE-USAGE-RC die ;

: SUITE-CHECK-ARGS ( -- )
   ARGC 3 > if 3 SCRIPT-SEP? 0= if SUITE-USAGE then then
   SCRIPT-ARGC 0 <> if SUITE-USAGE then ;

\ Each item names its own engine to its children: HABU_UNDER_TEST, which
\ src/lib/engine-candidate.f reads first, names the binary this item runs on, so
\ an outer value cannot send a whitebox item's children to the sealed product.
\ HABU_FIXPOINT_ENGINE goes with it: a suite that rebuilds an engine
\ (tools/build-fixpoint.f BF-ENGINE$) must land on the same one. Both are set
\ before the inherit, which skips a name already present.
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
   s" HABU_GATE_COMPILER_IMAGES" >LEN GT-ROOT >LEN PROC-ENV+
   PROC-ENV-INHERIT-MISSING ;

\ The spawn path is relative, as every spawn of it in the tree: the gate runs in
\ the checkout. The variable is absolute: test/boot-row-test.f spawns the engine
\ under a caller's CWD, where a relative path fails (measured: E-PROC-SPAWN,
\ exit 67). ENGINE-ID:PATH$ is this process's executable, so both name one file.
: PRODUCT-SPAWN$ ( -- ptr u8 n )
   s" bin/hb" ;

: PRODUCT-ENGINE$ ( -- ptr u8 n )
   ENGINE-ID:PATH$ ;

\ A row's argv waits with it: the readiness probe may start a build row, which
\ stages its own argv in the same process-wide table, so the tokens are kept
\ here and staged only once the row is ready to spawn.
: ROW-BEGIN ( -- )
   0 ROW-ARGS-U !
   0 ROW-ARG-N !
   0 0<> ROW-SCRIPT? !
   GATE-IMAGES:ROW-RESET ;

\ Every argv entry goes through as written, so a row measures what
\ `bin/hb --load <file>` measures; a file whose assertions are tier-1 facts
\ selects the tier itself, after harness and tool requires and before its
\ subject definitions.
: ROW-ARG+ ( ptr u8 n -- ) {: a:ptr u:n :}
   ROW-ARG-N @ 1+ ROW-ARG-LENS-RESERVE
   u ROW-ARG-N @ ROW-ARG-LENS !
   u 0 > if
      ROW-ARGS-U @ u + ROW-ARG-BYTES-RESERVE
      a ROW-ARGS-U @ ROW-ARG-BYTES u BYTE-COPY
   then
   ROW-ARGS-U @ u + ROW-ARGS-U !
   ROW-ARG-N @ 1+ ROW-ARG-N !
   a u s" --" STR= if 0 0= ROW-SCRIPT? ! exit then
   ROW-SCRIPT? @ 0= if a u GATE-IMAGES:ROW-FILE then ;

: ROW-ARGV! ( -- )
   PROC-ARGV-RESET
   s" --load" >LEN PROC-ARGV+
   0 ROW-SCAN !
   ROW-ARG-N @ 0 ?do
      i ROW-ARG-LENS @ {: u:n :}
      u 0 > if ROW-SCAN @ ROW-ARG-BYTES u else s" " then >LEN PROC-ARGV+
      ROW-SCAN @ u + ROW-SCAN !
   loop ;

\ TRUE once every keyed image the row needs passed, with its argv staged;
\ else the row is started as its own red row naming the failed build row.
\ Asked only once ROW-SETTLED? answered TRUE.
: SUITE-READY? ( ptr u8 n -- bool ) {: label:ptr labelu:n :}
   GATE-IMAGES:ROW-PASSED? if ROW-ARGV! 0 0= exit then
   label labelu GATE-IMAGES:ROW-RED
   0 0<> ;

\ Each runner answers FALSE, starting nothing, while the row's images are not
\ settled: the registry keeps the row pending (src/lib/test/suite.f). Once
\ settled it starts, or its red row does, and answers TRUE.
: SUITE-HB-RUN ( ptr u8 n -- bool ) {: label:ptr labelu:n :}
   GATE-IMAGES:ROW-SETTLED? 0= if false exit then
   label labelu SUITE-READY? 0= if true exit then
   PRODUCT-ENGINE$ GATE-IMAGES:ROW-GRANT$ SUITE-ENV
   PRODUCT-SPAWN$ label labelu GT-POOL-START
   true ;

\ A WHITEBOX-SUITE row runs on the gate's private copy of the unsealed engine,
\ which the whitebox-engine build row puts in place.
: SUITE-WB-RUN ( ptr u8 n -- bool ) {: label:ptr labelu:n :}
   GATE-IMAGES:ROW-WHITEBOX
   GATE-IMAGES:ROW-SETTLED? 0= if false exit then
   label labelu SUITE-READY? 0= if true exit then
   GATE-IMAGES:WHITEBOX$ GATE-IMAGES:ROW-GRANT$ SUITE-ENV
   GATE-IMAGES:WHITEBOX$ label labelu GT-POOL-START
   true ;

: SUITE-HB-RUN-STDIN ( ptr u8 n ptr u8 n -- bool ) {: in:ptr inu:n label:ptr labelu:n :}
   GATE-IMAGES:ROW-SETTLED? 0= if false exit then
   label labelu SUITE-READY? 0= if true exit then
   PRODUCT-ENGINE$ GATE-IMAGES:ROW-GRANT$ SUITE-ENV
   PRODUCT-SPAWN$ label labelu in inu GT-POOL-START-STDIN
   true ;

\ A keyed image's build row: the product engine, handed its program on stdin
\ with no arguments, granted the family it settles and that family's
\ prerequisites.
: SUITE-BUILD-RUN ( ptr u8 n ptr u8 n ptr u8 n -- )
   {: prog:ptr progu:n label:ptr labelu:n grant:ptr grantu:n :}
   PROC-ARGV-RESET
   PRODUCT-ENGINE$ grant grantu SUITE-ENV
   PRODUCT-SPAWN$ label labelu prog progu GT-POOL-START-STDIN ;

\ The entry guard walks the load graph DERIVE read and refuses the registry
\ before anything starts: this hook runs outside the catch around the rows. Each
\ keyed image settles in the pool through one build row; a row waits until its
\ images settle (test/gate-images.f), so a broken image closure fails its build
\ row and the rows that load it, and every other row runs.
: SUITE-SETUP ( -- )
   SUITE-CHECK-ARGS
   GATE-IMAGES:DERIVE
   ENTRY-GUARD:CHECK
   GT-POOL-CATCH-SIGNALS
   s" habu-native-suite" GT-START
   GT-POOL-RESET
   [: SUITE-BUILD-RUN ;] GATE-IMAGES:START ;

\ Before every drain the registry settles: every build row retires and every
\ waiting row starts, so a sequential group runs alone.
: SUITE-SETTLE ( -- )
   GATE-IMAGES:SETTLE-ALL ;

: SUITE-DRAIN ( -- )
   GT-POOL-DRAIN-SOFT ;

4096 constant COMPILER-CHUNK-CAP
create COMPILER-CHUNK COMPILER-CHUNK-CAP allot
variable COMPILER-OFF

: FORWARD-COMPILER-FILE ( ptr u8 n n -- )
   {: path:ptr pathu:n dest:n :}
   path pathu FS:>PATHZ open-rd {: fd:n :}
   fd 0 < if E-FS-OPEN throw then
   begin
      fd COMPILER-CHUNK COMPILER-CHUNK-CAP read {: u:n :}
      u 0 < if fd close E-FS-IO throw then
      u 0= if fd close exit then
      0 COMPILER-OFF !
      begin COMPILER-OFF @ u < while
         dest COMPILER-CHUNK COMPILER-OFF @ + u COMPILER-OFF @ - write
            {: wrote:n :}
         wrote 0 <= if fd close E-FS-IO throw then
         COMPILER-OFF @ wrote + COMPILER-OFF !
      repeat
   again ;

\ Pool captures contain the full streams, while their printed tails hold 32
\ KiB. Forward both complete streams before normal pool cleanup removes them.
: FORWARD-COMPILER-LOGS ( -- )
   GATE-IMAGES:COMPILER-RED {: red:n :}
   red 0 < if exit then
   s" compiler-images stdout (full):" type cr
   red GT-POOL-RED-OUT$ 1 FORWARD-COMPILER-FILE
   2 S\" compiler-images stderr (full):\n" write drop
   red GT-POOL-RED-ERR$ 2 FORWARD-COMPILER-FILE ;

\ The pool drains softly between groups, so every suite runs whatever went red
\ before it; the red set is reported once here, with exit codes, and decides the
\ exit status. Cleanup runs before the red die ends the process, else each red
\ run leaves its pool root in /tmp; every red row's output and capture names are
\ already on stdout. A throwing cleanup is named GT-POOL-CLEANUP.
: SUITE-FINISH ( n -- ) {: rc:n :}
   s" suites: ran " type TEST:ITEMS-RUN GT-U-TYPE
   s"  of " type TEST:ITEMS-REGISTERED GT-U-TYPE cr
   GT-POOL-RED-REPORT
   FORWARD-COMPILER-LOGS
   GT-POOL-CLEANUP
   GT-POOL-SIGNAL-CHECK
   rc 0 <> if exit then                 \ the body threw: the framework rethrows that code
   GT-POOL-RED# 0 > if s" test pool failed" 1 die then ;

: SUITE-INSTALL-HOOKS ( -- )
   [: SUITE-SETUP ;] TEST:SETUP!
   [: SUITE-FINISH ;] TEST:TEARDOWN!
   [: SUITE-SETTLE ;] TEST:SETTLE!
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
