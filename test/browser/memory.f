\ memory.f - BROWSER-MEMORY-TEST: the memory src/lib/browser/host-cli.mjs's
\ --memory line reports, for the browser tests that check a module frees its
\ deliveries. FREED runs a module once and four times with the same steps and
\ asserts both runs end with the same memory; MEMORY reads the line from a
\ test's own captured output. bun must be on PATH.

require src/lib/string.f
require src/lib/fs.f
require src/lib/process.f
require src/lib/process-argv.f
require src/lib/process-env.f
require src/lib/test.f

package BROWSER-MEMORY-TEST
private

60000 constant DEADLINE-MS
8192 constant CAP
CAP BUFFER: OUT
CAP BUFFER: ERR
PATH-CAP BUFFER: EXE

: ARG+ ( ptr u8 n -- ) >LEN PROC-ARGV+ ;

public

\ The number on the "memory" line host-cli's --memory prints after the
\ output's other lines; E-PROC-OUTPUT without one.
: MEMORY ( ptr u8 n -- n )
   {: out:ptr u:n :}
   out u s\" \nmemory " FIND-SUB MATCH option
      none OF E-PROC-OUTPUT throw ENDOF
      some OF IDX>N ENDOF
   ;MATCH 8 + {: at:n :}
   out at + u at - TRIM STR>NUMBER? MATCH option
      none OF E-PROC-OUTPUT throw ENDOF
      some OF ENDOF
   ;MATCH ;

private

\ The memory after host-cli runs the module at the path with the steps staged
\ count times; the run's exit is checked under the label, its stderr shown when
\ it failed.
: MEMORY-AFTER ( ptr u8 n ptr u8 n [ -- ] n -- n )
   {: label:ptr lu:n mod:ptr mu:n steps count:n :}
   PROC-ARGV-RESET
   s" src/lib/browser/host-cli.mjs" ARG+ mod mu ARG+
   count 0 ?do steps execute loop
   s" --memory" ARG+
   s" bun" >LEN EXE RESOLVE-EXECUTABLE {: eu:len :}
   EXE eu OUT CAP >LEN ERR CAP >LEN DEADLINE-MS >MS
   RUN-ARGV-CAPTURE-OUTCOME PROC-OUTCOME>RC RC>N
   {: outu:len erru:len rc:n :}
   rc 0<> if ERR erru LEN>N type cr then
   label lu T-LABEL rc 0 T=
   OUT outu LEN>N MEMORY ;

public

: FREED ( ptr u8 n ptr u8 n [ -- ] -- )
   {: label:ptr lu:n mod:ptr mu:n steps :}
   label lu mod mu steps 1 MEMORY-AFTER {: one:n :}
   label lu mod mu steps 4 MEMORY-AFTER {: four:n :}
   label lu T-LABEL four one T= ;

;package
