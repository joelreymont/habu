\ Grow past the fixed band without losing either old or new declarations, and
\ run a linked program whose code cell `is` stored twice: it holds one row.
require src/core/exit-status.f
require src/lib/string.f
require src/lib/test.f
require src/lib/fs.f
require src/lib/fs-mutate.f
require src/lib/process.f
require src/lib/process-command.f
require src/lib/engine-candidate.f
require src/habu/address-cells.f

package ADDRESS-CELL-CAP-GROWN

75900 constant ROW-N
create SLOTS ROW-N cells allot
variable BASE-N

: ROWS ( -- n ) ADDRESS-CELLS:LIVE-SPAN nip ;

: GROWN ( -- )
   ROWS dup BASE-N !
   ROW-N >= if s" address-cell-growth: fixture has no new rows" EXIT-STATUS:FAILURE die then
   ROW-N BASE-N @ - 0 ?do SLOTS i cells + ptr-cell-mark loop
   ROWS ROW-N T=
   SLOTS ptr-cell-mark
   SLOTS ROW-N BASE-N @ - 1- cells + ptr-cell-mark
   ROWS ROW-N T= ;

PATH-CAP BUFFER: DIR  variable DIR-U
PATH-CAP BUFFER: PATH
: DIR$ ( -- str ) DIR DIR-U @ >str ;
\ NAME's path in DIR, in bytes the next call reuses.
: IN$ ( str -- str ) DIR$ swap PATH FS:JOIN-PATH PATH swap >str ;

: SHOW ( CMD:command n -- CMD:command ) {: rc:n :}
   s" address-cell-cap-grown: exited " type rc .
   s" stdout: " type CMD:OUT$ type cr  s" stderr: " type CMD:ERR$ type cr ;

: RUN-RC ( CMD:command -- CMD:command n )
   ENGINE-CANDIDATE:PATH$ PROCESS:WAIT-FOREVER CMD:RUN-OUTCOME
   PROCESS:ENDING>RC RC>N ;

: WRITE-PROG ( -- )
   s" prog.f" IN$ S\" defer ACG-D ( -- n )\n" FS:WRITE-ALL
   s" prog.f" IN$ S\" : ACG-SET ( -- ) [: 6 ;] is ACG-D  [: 7 ;] is ACG-D ;\nACG-SET\n" FS:APPEND-FILE
   s" prog.f" IN$ S\" : MAIN ( -- ) ACG-D . ;\n" FS:APPEND-FILE ;

: LINKED ( CMD:command -- CMD:command )
   s" a code cell stored twice links and runs with its last value" T-LABEL
   WRITE-PROG
   CMD:RESET
   s" --load" CMD:ARG+  s" tools/hb-build.f" CMD:ARG+  s" --" CMD:ARG+
   s" prog.f" IN$ CMD:ARG+  s" -o" CMD:ARG+  s" prog" IN$ CMD:ARG+
   s" HABU_OUT" s" out" IN$ CMD:ENV+
   RUN-RC {: built:n :}
   built 0<> if built SHOW then
   built 0 T=  built 0<> if exit then
   CMD:RESET  s" prog" IN$ PROCESS:WAIT-FOREVER CMD:RUN-OUTCOME
   PROCESS:ENDING>RC RC>N {: rc:n :}
   rc 0<> if rc SHOW then
   CMD:OUT$ s\" 7\n" T$=
   rc 0 T= ;

: CHECKS ( -- ) [: LINKED ;] CMD:WITH-COMMAND ;
: REMOVE ( -- ) DIR$ FS:REMOVE-TREE ;

: RUN ( -- )
   T-RESET
   GROWN
   s" address-cell-cap-grown" FS:MKTEMP-DIR {: s:str :}
   s STR:DATA DIR s STR:LEN BYTE-COPY  s STR:LEN DIR-U !
   ['] CHECKS ['] REMOVE finally
   T-REPORT ;

RUN
;package
