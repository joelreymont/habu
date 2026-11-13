\ harness.f - WASM-HARNESS: a module file checked by two engines independent of
\ Habu, wasm-tools validating it and bun running it through test/wasm/run.mjs.
\ Both are found on PATH and run from the tree's root; a missing tool throws.
\
\ BUILD answers `hb-build --target wasm`'s exit status, run from the tree's root.
\ VALID? answers wasm-tools validate's verdict under core Wasm and WPROF's
\ features. RUN answers run.mjs's exit status: 0 or 1, the entry's status, 2 for
\ a trap and 3 for a compile or link error. OUT$ answers the OUT bytes the run
\ wrote and THROW-CODE its full throw code, 0 unless it exited 1; FAULT-KIND and
\ FAULT-ADDR the kind and address of the fault that trapped or threw uncaught,
\ else 0.

require src/lib/string.f
require src/lib/fs.f
require src/lib/process.f
require src/lib/process-argv.f
require src/lib/process-env.f
require src/arch/wasm/profile.f

package WASM-HARNESS
private

4096 constant ERR-CAP

PATH-CAP BUFFER: EXE
WPROF:OUT-CAP BUFFER: OUT
ERR-CAP BUFFER: ERR
variable OUT-U
variable ERR-U
variable CODE
variable KIND
variable ADDR

: ARG+ ( ptr u8 n -- )  >LEN PROC-ARGV+ ;

\ The program at the path, run with the arguments staged; answers its exit
\ status, or 128 plus the signal that ended it.
: RUN-AT ( ptr u8 len -- n )
   OUT WPROF:OUT-CAP >LEN  ERR ERR-CAP >LEN  PROCESS:WAIT-FOREVER
   RUN-ARGV-CAPTURE-OUTCOME PROCESS:ENDING>RC RC>N {: outu:len erru:len rc:n :}
   outu LEN>N OUT-U !  erru LEN>N ERR-U !
   rc ;

: SPAWN ( ptr u8 n -- n )
   >LEN EXE RESOLVE-EXECUTABLE {: u:len :}
   EXE u RUN-AT ;

: FIRST-LINE ( -- ptr u8 n )
   ERR ERR-U @ RTRIM {: a:ptr u:n :}
   a u 10 INDEX-OF MATCH option
      some OF IDX>N a swap ENDOF
      none OF a u ENDOF
   ;MATCH ;

\ run.mjs's first stderr line for status 1 is its throw line; bun's own failures
\ exit 1 as well, so any other text ends the check.
: NO-THROW-LINE ( ptr u8 n -- )
   type cr
   s" wasm harness: bun exited 1 without a `throw <i64>` line" 1 die ;

: THROWN ( -- n )
   FIRST-LINE {: a:ptr u:n :}
   a u s" throw " STARTS-WITH? 0= if  a u NO-THROW-LINE  then
   a 6 + u 6 - STR>NUMBER? MATCH option
      some OF ENDOF
      none OF a u NO-THROW-LINE ENDOF
   ;MATCH ;

\ run.mjs's last stderr line for status 2 is its fault line, and for status 1
\ when it reports one; bun's own failures may exit 2 as well, so any other text
\ ends the check.
: NO-FAULT-LINE ( ptr u8 n -- )
   type cr
   s" wasm harness: bun's stderr ends without a `fault <kind> <addr>` line" 1 die ;

: LAST-LINE ( -- ptr u8 n )
   ERR ERR-U @ RTRIM {: a:ptr u:n :}
   u begin dup 0 > while
      dup 1- a + c@ 10 = if  a over +  u rot -  exit  then
      1-
   repeat drop a u ;

: FAULT-FIELD ( ptr u8 n ptr u8 n -- n )
   {: line:ptr lu:n a:ptr u:n :}
   a u STR>NUMBER? MATCH option
      some OF ENDOF
      none OF line lu NO-FAULT-LINE ENDOF
   ;MATCH ;

: FAULTED ( -- )
   LAST-LINE {: a:ptr u:n :}
   a u s" fault " STARTS-WITH? 0= if  a u NO-FAULT-LINE  then
   a 6 + u 6 - 32 INDEX-OF MATCH option
      some OF IDX>N ENDOF
      none OF a u NO-FAULT-LINE ENDOF
   ;MATCH {: k:n :}
   a u  a 6 + k FAULT-FIELD KIND !
   a u  a 7 + k +  u 7 - k -  FAULT-FIELD ADDR ! ;

\ A status 1 run reports a fault only when the uncaught throw was one.
: FAULT-REPORTED? ( -- bool )  LAST-LINE s" fault " STARTS-WITH? ;

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

: BUILD ( ptr u8 n ptr u8 n -- n )
   {: src:ptr su:n mod:ptr mu:n :}
   PROC-ARGV-RESET
   s" --load" ARG+  s" tools/hb-build.f" ARG+  s" --" ARG+  s" --target" ARG+  s" wasm" ARG+
   src su ARG+  s" -o" ARG+  mod mu ARG+
   s" bin/hb" >LEN RUN-AT {: rc:n :}
   rc 0<> if  ERR ERR-U @ type cr  then
   rc ;

: VALID? ( ptr u8 n -- bool )  false VALIDATE ;

\ W05 probes one missing required feature on the same generated module.
: VALID-WITHOUT-SAT? ( ptr u8 n -- bool )  true VALIDATE ;

: RUN ( ptr u8 n -- n )
   {: path:ptr u:n :}
   PROC-ARGV-RESET
   s" test/wasm/run.mjs" ARG+  path u ARG+
   s" bun" SPAWN {: rc:n :}
   0 CODE !  0 KIND !  0 ADDR !
   rc 1 = if  THROWN CODE !  FAULT-REPORTED? if  FAULTED  then  then
   rc 2 = if  FAULTED  then
   rc ;

: OUT$ ( -- ptr u8 n )  OUT OUT-U @ ;

: THROW-CODE ( -- n )  CODE @ ;
: FAULT-KIND ( -- n )  KIND @ ;
: FAULT-ADDR ( -- n )  ADDR @ ;

;package
