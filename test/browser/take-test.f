\ take-test.f - test/browser/take.f's module under bun: a delivery a turn takes
\ with BROWSER-EVENT:TAKE (src/lib/browser/event.f) outlives the turn, and the
\ module frees it in a later one. The module is built by tools/wasm-build.f's
\ command line and validated by wasm-tools. bun and wasm-tools must be on PATH,
\ so this is no row of the ordinary gate: test/wasm/device.f runs it, as does
\ `bin/hb --load test/browser/take-test.f` from the tree's root.
\
\ - taken: typed "take" answers the text in its taken pages after its run, with
\   DELIVERY and the event's fields as they were before TAKE; the next typed
\   turn, whose delivery HANDLE frees, reads "take" back from those pages.
\ - thrown: typed "boom" takes its delivery and throws 93; the next turn still
\   reads "boom" from it.
\ - freed: four take-then-free cycles end with the memory one does, so the
\   taken pages are freed and a turn after a take frees its own delivery.

require src/lib/string.f
require src/lib/fs.f
require src/lib/fs-mutate.f
require src/lib/process.f
require src/lib/process-argv.f
require src/lib/process-env.f
require src/lib/test.f
require test/wasm/harness.f
require test/browser/memory.f

package BROWSER-TAKE-TEST
private

60000 constant DEADLINE-MS
4096 constant CAP
CAP BUFFER: OUT
CAP BUFFER: ERR
variable OUT-U
variable ERR-U
FS-PATH-CAP BUFFER: EXE
FS-PATH-CAP BUFFER: DIR
variable DIR-U
FS-PATH-CAP BUFFER: MOD
variable MOD-U

: ARG+ ( ptr u8 n -- ) >LEN PROC-ARGV+ ;

: SPAWN ( ptr u8 len -- n )
   OUT CAP >LEN ERR CAP >LEN DEADLINE-MS >MS
   RUN-ARGV-CAPTURE-OUTCOME PROC-OUTCOME>RC RC>N
   {: outu:len erru:len rc:n :}
   outu LEN>N OUT-U ! erru LEN>N ERR-U ! rc ;

: SETUP ( -- )
   s" browser-take" TMPDIR-MKDIR {: a:ptr u:n :}
   a DIR u BYTE-COPY u DIR-U !
   s" browser take: module in " type DIR u type cr
   DIR u s" take.wasm" MOD JOIN-PATH MOD-U ! ;

: BUILD ( -- n )
   PROC-ARGV-RESET
   s" --load" ARG+ s" tools/wasm-build.f" ARG+ s" --" ARG+
   s" test/browser/take.f" ARG+ s" TAKE-TURN:TURN" ARG+ MOD MOD-U @ ARG+
   s" bin/hb" >LEN SPAWN ;

\ host-cli.mjs on the module with the typed texts, run by bun.
: HOST ( ptr u8 n ptr u8 n -- n )
   {: a:ptr au:n b:ptr bu:n :}
   PROC-ARGV-RESET
   s" src/lib/browser/host-cli.mjs" ARG+ MOD MOD-U @ ARG+
   s" --typed" ARG+ a au ARG+  s" --typed" ARG+ b bu ARG+
   s" bun" >LEN EXE RESOLVE-EXECUTABLE {: u:len :}
   EXE u SPAWN ;

: TAKEN-CASE ( -- )
   s" take" s" next" HOST {: rc:n :}
   s" taken: the taken pages answer its text after the run and in the next turn" T-LABEL
   OUT OUT-U @ S\" hello\ntext take\ntext held take\n" T$=
   s" taken: DELIVERY and the event's fields read the same after TAKE" T-LABEL
   ERR ERR-U @ s" " T$=
   rc 0 T= ;

: THROWN-CASE ( -- )
   s" boom" s" next" HOST {: rc:n :}
   s" thrown: a turn that throws after TAKE leaves its delivery taken" T-LABEL
   OUT OUT-U @ S\" hello\ntext held boom\n" T$=
   ERR ERR-U @ S\" throw 93\n" T$=
   rc 1 T= ;

public

: RUN ( -- )
   T-RESET SETUP
   s" browser take fixture builds" T-LABEL
   BUILD {: rc:n :}
   rc 0 T=
   rc 0<> if ERR ERR-U @ type cr T-REPORT exit then
   s" browser take fixture validates" T-LABEL
   MOD MOD-U @ WASM-HARNESS:VALID? TTRUE
   TAKEN-CASE
   THROWN-CASE
   s" four take-then-free cycles end with the memory one does"
      MOD MOD-U @ [: s" --typed" ARG+ s" take" ARG+ s" --typed" ARG+ s" next" ARG+ ;]
      BROWSER-MEMORY-TEST:FREED
   T-REPORT ;

;package

BROWSER-TAKE-TEST:RUN
