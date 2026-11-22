split-str
\ harness.f - WASM-HARNESS: a module file checked by two engines independent of
\ Habu, wasm-tools validating it and bun running it through test/wasm/run.mjs.
\ Both are found on PATH and run from the tree's root; a missing tool throws.
\
\ BUILD answers `hb-build --target wasm`'s exit status, run from the tree's root.
\ VALID? answers wasm-tools validate's verdict under core Wasm and WPROF's
\ features. RUN answers run.mjs's exit status: 0 or 1, the entry's status, 2 for
\ a trap and 3 for a compile or link error. OUT$ answers the OUT bytes the run
\ wrote and ERR$ the ERR bytes, the stderr after run.mjs's own lines; THROW-CODE
\ its full throw code, 0 unless it exited 1; FAULT-KIND and FAULT-ADDR the kind
\ and address of the fault that trapped, else 0; MEMORY-START and MEMORY-END the
\ module's memory bytes before its run and after it, 0 for a module that did not
\ link.

require src/core/exit-status.f
require src/lib/string.f
require src/lib/fs.f
require src/lib/process.f
require src/lib/process-argv.f
require src/lib/process-env.f
require src/arch/wasm/profile.f

package WASM-HARNESS
private

WPROF:OUT-CAP 4 * constant OUT-BYTES      \ the most OUT bytes a run's capture keeps
WPROF:ERR-CAP 4 * constant ERR-BYTES      \ the most stderr bytes a run's capture keeps

PATH-CAP BUFFER: EXE
OUT-BYTES BUFFER: OUT
ERR-BYTES BUFFER: ERR
variable OUT-U
variable ERR-U
variable TAIL                             \ where in ERR run.mjs's own lines end
variable CODE
variable KIND
variable ADDR
variable MEM-START
variable MEM-END

: ARG+ ( ^u8 n -- )  >LEN PROC-ARGV+ ;

\ The program at the path, run with the arguments staged; answers its exit
\ status, or 128 plus the signal that ended it.
: RUN-AT ( ^u8 len -- n )
   LEN>N OUT OUT-BYTES >LEN  ERR ERR-BYTES >LEN  PROCESS:WAIT-FOREVER
   RUN-ARGV-CAPTURE-OUTCOME PROCESS:ENDING>RC RC>N {: outu:len erru:len rc:n :}
   outu LEN>N OUT-U !  erru LEN>N ERR-U !
   rc ;

: SPAWN ( ^u8 n -- n )
   EXE RESOLVE-EXECUTABLE {: u:len :}
   EXE u RUN-AT ;

: FIRST-LINE ( -- str )
   ERR ERR-U @ RTRIM {: a:^ u:n :}
   a u 10 INDEX-OF MATCH option
      some OF IDX>N a swap ENDOF
      none OF a u ENDOF
   ;MATCH ;

\ run.mjs's first stderr line for status 1 is its throw line; bun's own failures
\ exit 1 as well, so any other text ends the check.
: NO-THROW-LINE ( str -- )
   type cr
   s" wasm harness: bun exited 1 without a `throw <i64>` line" EXIT-STATUS:FAILURE die ;

: THROWN ( -- n )
   FIRST-LINE {: a:^ u:n :}
   a u s" throw " STARTS-WITH? 0= if  a u NO-THROW-LINE  then
   a 6 + u 6 - STR>NUMBER? MATCH option
      some OF ENDOF
      none OF a u NO-THROW-LINE ENDOF
   ;MATCH ;

\ run.mjs's stderr line after its memory line for status 2 is its fault line;
\ bun's own failures may exit 2 as well, so any other text ends the check.
: NO-FAULT-LINE ( str -- )
   type cr
   s" wasm harness: bun's memory line is not followed by a `fault <kind> <addr>` line"
   EXIT-STATUS:FAILURE die ;

: FAULT-FIELD ( str str -- n )
   {: line:^ lu:n a:^ u:n :}
   a u STR>NUMBER? MATCH option
      some OF ENDOF
      none OF line lu NO-FAULT-LINE ENDOF
   ;MATCH ;

: FAULTED ( -- )
   ERR ERR-U @ 10 TAIL @ SPLIT-NEXT drop {: a:^ u:n next:n :}
   a u s" fault " STARTS-WITH? 0= if  ERR ERR-U @ NO-FAULT-LINE  then
   a 6 + u 6 - 32 INDEX-OF MATCH option
      some OF IDX>N ENDOF
      none OF a u NO-FAULT-LINE ENDOF
   ;MATCH {: k:n :}
   a u  a 6 + k FAULT-FIELD KIND !
   a u  a 7 + k +  u 7 - k -  FAULT-FIELD ADDR !
   next ERR-U @ min TAIL ! ;

\ run.mjs writes `memory <start> <end>` on a stderr line of its own; any other
\ text after `memory ` ends the check.
: NO-MEMORY-LINE ( str -- )
   type cr
   s" wasm harness: run.mjs wrote a `memory` line without two numbers" EXIT-STATUS:FAILURE die ;

: MEMORY-FIELD ( str str -- n )
   {: line:^ lu:n a:^ u:n :}
   a u STR>NUMBER? MATCH option
      some OF ENDOF
      none OF line lu NO-MEMORY-LINE ENDOF
   ;MATCH ;

\ True at run.mjs's memory line, whose next line starts at next: TAIL moves there.
: MEMORY? ( str n -- bool )
   {: a:^ u:n next:n :}
   a u s" memory " STARTS-WITH? 0= if false exit then
   a 7 + u 7 - 32 INDEX-OF MATCH option
      some OF IDX>N ENDOF
      none OF a u NO-MEMORY-LINE ENDOF
   ;MATCH {: k:n :}
   a u  a 7 + k MEMORY-FIELD MEM-START !
   a u  a 8 + k +  u 8 - k -  MEMORY-FIELD MEM-END !
   next ERR-U @ min TAIL !  true ;

\ The first memory line is run.mjs's: ERR bytes after it may hold another.
: MEMORY ( -- )
   0 MEM-START !  0 MEM-END !  ERR-U @ TAIL !
   0 begin  >r ERR ERR-U @ 10 r> SPLIT-NEXT  while
      dup >r MEMORY? if  r> drop exit  then  r>
   repeat drop drop drop ;

: VALIDATE ( ^u8 n bool -- bool )
   {: path:^ u:n no-sat:bool :}
   PROC-ARGV-RESET
   s" validate" ARG+
   s" --features" ARG+  s" mvp" ARG+          \ each list adds to the last
   s" --features" ARG+  WPROF:FEATURES ARG+
   no-sat if  s" --features=-saturating-float-to-int" ARG+  then
   path u ARG+
   s" wasm-tools" SPAWN {: rc:n :}
   rc 1 > if
      ERR ERR-U @ type cr
      s" wasm harness: wasm-tools validate failed" EXIT-STATUS:FAILURE die
   then
   rc 0= ;

public

: BUILD ( ^u8 n ^u8 n -- n )
   {: src:^ su:n mod:^ mu:n :}
   PROC-ARGV-RESET
   s" --load" ARG+  s" tools/hb-build.f" ARG+  s" --" ARG+  s" --target" ARG+  s" wasm" ARG+
   src su ARG+  s" -o" ARG+  mod mu ARG+
   s" bin/hb" >LEN RUN-AT {: rc:n :}
   rc 0<> if  ERR ERR-U @ type cr  then
   rc ;

: VALID? ( ^u8 n -- bool )  false VALIDATE ;

\ W05 probes one missing required feature on the same generated module.
: VALID-WITHOUT-SAT? ( str -- bool )  true VALIDATE ;

: RUN ( ^u8 n -- n )
   {: path:^ u:n :}
   PROC-ARGV-RESET
   s" test/wasm/run.mjs" ARG+  path u ARG+
   s" bun" SPAWN {: rc:n :}
   0 CODE !  0 KIND !  0 ADDR !
   rc 1 = if  THROWN CODE !  then
   MEMORY
   rc 2 = if  FAULTED  then
   rc ;

: OUT$ ( -- ^u8 n )  OUT OUT-U @ ;
: ERR$ ( -- ^u8 n )  ERR TAIL @ +  ERR-U @ TAIL @ - ;

: THROW-CODE ( -- n )  CODE @ ;
: FAULT-KIND ( -- n )  KIND @ ;
: FAULT-ADDR ( -- n )  ADDR @ ;
: MEMORY-START ( -- n )  MEM-START @ ;
: MEMORY-END ( -- n )  MEM-END @ ;

;package
