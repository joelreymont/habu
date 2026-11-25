\ Fixed storage binds the accessor that owns its allocation. A linked program
\ runs the quotation and reads through the address its build stored in typed
\ cells; hb-build.f reads its argv, so it is spawned from the tree's root.
require src/lib/string.f
require src/lib/test/assert.f
require src/lib/fs.f
require src/lib/fs-mutate.f
require src/lib/process.f
require src/lib/process-command.f
require src/lib/engine-candidate.f

package STORAGE-BIND-TEST

T-RESET

STRUCTURE plain-row
   FIELD first n
   FIELD second ^u8
;STRUCTURE

4096 TYPED-BUFFER PLAIN plain-row

private
TYPED-VARIABLE ROW [ n -- n ]
: PRIVATE-STORE ( -- ) [: 1 + ;] ROW ! ;
: PRIVATE-CALL ( n -- n ) ROW @ execute ;

public
\ The public accessor has the same tail as the private quote accessor.
TYPED-VARIABLE ROW n
: PUBLIC-STORE ( n -- ) STORAGE-BIND-TEST:ROW ! ;
: PUBLIC-READ ( -- n ) STORAGE-BIND-TEST:ROW @ ;

: RUN ( -- )
   17 PUBLIC-STORE PUBLIC-READ 17 T=
   PRIVATE-STORE 5 PRIVATE-CALL 6 T=
   3 s" abc" STR:DATA PLAIN-ROW-MAKE 4095 PLAIN !
   4095 PLAIN @ PLAIN-ROW-UNMAKE 3 >str
   s" abc" T$= 3 T=
   5 PRIVATE-CALL 6 T= ;

RUN

private
PATH-CAP BUFFER: DIR   variable DIR-U
PATH-CAP BUFFER: SRC   variable SRC-U
PATH-CAP BUFFER: PROG  variable PROG-U
PATH-CAP BUFFER: OUT   variable OUT-U
: DIR$ ( -- str ) DIR DIR-U @ >str ;
: SRC$ ( -- str ) SRC SRC-U @ >str ;
: PROG$ ( -- str ) PROG PROG-U @ >str ;
: OUT$ ( -- str ) OUT OUT-U @ >str ;

\ The build stores a quotation and the address of a cell it set to 7.
: PROGRAM-SOURCE$ ( -- str )
   S\" require src/lib/adt/nullable.f\nTYPED-VARIABLE SBT-H [ n -- n ]\nTYPED-VARIABLE SBT-N n\nTYPED-VARIABLE SBT-P nullable<u8>\n: SBT-SET ( -- ) [: 1 + ;] SBT-H !  7 SBT-N !  SBT-N BYTE-VIEW NULLABLE:SOME SBT-P ! ;\nSBT-SET\n: SBT-BYTE ( -- n ) SBT-P @ MATCH nullable none OF 0 ENDOF some OF c@ ENDOF ;MATCH ;\n: MAIN ( -- ) 41 SBT-H @ execute . SBT-BYTE . ;\n" ;

: SHOW ( CMD:command n -- CMD:command )
   {: rc:n :}
   s" storage-binding: exited " type rc .
   s" stdout: " type CMD:OUT$ type cr  s" stderr: " type CMD:ERR$ type cr ;

: BUILD ( CMD:command -- CMD:command n )
   CMD:RESET
   s" --load" CMD:ARG+  s" tools/hb-build.f" CMD:ARG+  s" --" CMD:ARG+
   SRC$ CMD:ARG+  s" -o" CMD:ARG+  PROG$ CMD:ARG+
   s" HABU_OUT" OUT$ CMD:ENV+
   ENGINE-CANDIDATE:PATH$ PROCESS:WAIT-FOREVER CMD:RUN-OUTCOME
   PROCESS:ENDING>RC RC>N ;

: LINKED-CASE ( CMD:command -- CMD:command )
   SRC$ PROGRAM-SOURCE$ FS:WRITE-ALL
   BUILD {: built:n :}
   built 0<> if built SHOW then
   s" the typed-cell program builds" T-LABEL  built 0 T=
   built 0<> if exit then
   CMD:RESET  PROG$ PROCESS:WAIT-FOREVER CMD:RUN-OUTCOME
   PROCESS:ENDING>RC RC>N {: rc:n :}
   rc 0<> if rc SHOW then
   s" the linked program runs its build's quotation and reads its build's address" T-LABEL
   CMD:OUT$ s\" 42\n7\n" T$=
   s" the linked program exits 0" T-LABEL  rc 0 T= ;

: REMOVE ( -- ) DIR$ FS:REMOVE-TREE ;

: LINKED ( -- )
   s" habu-storage-binding" FS:MKTEMP-DIR {: d:str :}
   d STR:DATA DIR d STR:LEN BYTE-COPY  d STR:LEN DIR-U !
   DIR$ s" cells.f" SRC FS:JOIN-PATH SRC-U !
   DIR$ s" cells" PROG FS:JOIN-PATH PROG-U !
   DIR$ s" out" OUT FS:JOIN-PATH OUT-U !
   [: [: LINKED-CASE ;] CMD:WITH-COMMAND ;] ['] REMOVE finally ;

LINKED
T-REPORT
;package
