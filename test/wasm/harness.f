\ harness.f - WASM-HARNESS: a module file checked by two engines independent of
\ Habu, wasm-tools validating it and bun running it through test/wasm/run.mjs
\ (docs/wasm-backend.md 17.6). Both are found on PATH and run from the tree's
\ root; a missing tool or a passed deadline throws lib/process.f's code.
\
\ VALID? answers wasm-tools validate's verdict under core Wasm and WPROF's
\ features. RUN answers run.mjs's exit status: 0 or 1, the entry's status, 2 for
\ a trap and 3 for a compile or link error. OUT$ answers the OUT bytes the run
\ wrote and THROW-CODE its full throw code, 0 unless it exited 1.

require lib/string.f
require lib/fs.f
require lib/process.f
require lib/process-argv.f
require lib/process-env.f
require src/arch/wasm/profile.f

package WASM-HARNESS
private

60000 constant DEADLINE-MS
WPROF:STACK-BASE WPROF:OUT-BASE - constant OUT-CAP   \ the OUT region, all a run writes
4096 constant ERR-CAP

FS-PATH-CAP BUFFER: EXE
OUT-CAP BUFFER: OUT
ERR-CAP BUFFER: ERR
variable OUT-U
variable ERR-U
variable CODE

: ARG+ ( ptr u8 n -- )  >LEN PROC-ARGV+ ;

\ The tool named, run with the arguments staged after PROC-ARGV-RESET; answers
\ its exit status, or 128 plus the signal that ended it.
: SPAWN ( ptr u8 n -- n )
   >LEN EXE RESOLVE-EXECUTABLE {: u:len :}
   EXE u  OUT OUT-CAP >LEN  ERR ERR-CAP >LEN  DEADLINE-MS >MS
   RUN-ARGV-CAPTURE-OUTCOME PROC-OUTCOME>RC RC>N {: outu:len erru:len rc:n :}
   outu LEN>N OUT-U !  erru LEN>N ERR-U !
   rc ;

\ run.mjs's stderr for status 1 is its throw line; bun's own failures exit 1 as
\ well, so any other text ends the check.
: NO-THROW-LINE ( ptr u8 n -- )
   type cr
   s" wasm harness: bun exited 1 without a `throw <i64>` line" 1 die ;

: THROWN ( -- n )
   ERR ERR-U @ RTRIM {: a:ptr u:n :}
   a u s" throw " STARTS-WITH? 0= if  a u NO-THROW-LINE  then
   a 6 + u 6 - STR>NUMBER? MATCH option
      some OF ENDOF
      none OF a u NO-THROW-LINE ENDOF
   ;MATCH ;

: VALIDATE ( ptr u8 n bool -- bool )
   {: path:ptr u:n no-sat:bool :}
   PROC-ARGV-RESET
   s" validate" ARG+
   s" --features" ARG+  s" mvp" ARG+          \ each list adds to the last
   s" --features" ARG+  WPROF:FEATURES ARG+
   no-sat if  s" --features=-saturating-float-to-int" ARG+  then
   path u ARG+
   s" wasm-tools" SPAWN {: rc:n :}
   rc 1 > if
      ERR ERR-U @ type cr
      s" wasm harness: wasm-tools validate failed" 1 die
   then
   rc 0= ;

public

: VALID? ( ptr u8 n -- bool )  false VALIDATE ;

\ W05 probes one missing required feature on the same generated module.
: VALID-WITHOUT-SAT? ( ptr u8 n -- bool )  true VALIDATE ;

: RUN ( ptr u8 n -- n )
   {: path:ptr u:n :}
   PROC-ARGV-RESET
   s" test/wasm/run.mjs" ARG+  path u ARG+
   s" bun" SPAWN {: rc:n :}
   0 CODE !
   rc 1 = if  THROWN CODE !  then
   rc ;

: OUT$ ( -- ptr u8 n )  OUT OUT-U @ ;

: THROW-CODE ( -- n )  CODE @ ;

;package
