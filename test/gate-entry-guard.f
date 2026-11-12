\ Gate entry files are execution roots. A row may share an inert helper, but
\ importing or launching another row's entry silently runs that suite twice.
\
\ The guard walks the load graph, from every file a running row loads through
\ every import and launch. A load of another running row's entry is refused,
\ naming the file and line that makes it, and so is a preload that is another
\ running row's entry. A file may name itself, and rows may share an entry.

require src/lib/error.f
require src/lib/fmt.f
require src/lib/test.f
require test/load-graph.f

package ENTRY-GUARD
private

DYNAMIC-BUFFER ROW-A n                   \ the first row a file is an entry of
DYNAMIC-BUFFER ROW-B n                   \ another row it is an entry of
DYNAMIC-BUFFER MARK n                    \ the row walk that last reached it
DYNAMIC-BUFFER QUEUE n
variable WALK
variable QHEAD
variable QTAIL
variable OWNER
variable ERRORS

: RESET ( -- )
   LOAD-GRAPH:FILE-COUNT {: n:n :}
   n ROW-A-RESERVE n ROW-B-RESERVE n MARK-RESERVE n QUEUE-RESERVE
   n 0 ?do -1 i ROW-A ! -1 i ROW-B ! 0 i MARK ! loop
   0 WALK ! -1 OWNER ! 0 ERRORS ! ;

: ENTRY-ADD ( n bool ptr u8 n -- ) {: row:n entry:bool a:ptr u:n :}
   a u LOAD-GRAPH:FILE-ID {: f:n :}
   entry 0= if exit then
   f ROW-A @ 0 < if row f ROW-A ! exit then
   f ROW-A @ row <> if row f ROW-B ! then ;

\ A row other than the owner that has the file as an entry, else -1.
: OTHER ( n -- n ) {: f:n :}
   f ROW-A @ OWNER @ <> if f ROW-A @ exit then
   f ROW-B @ ;

: REPORT ( n n n -- ) {: src:n e:n row:n :}
   e LOAD-GRAPH:EDGE-TO {: to:n :}
   s" entry guard: " type src LOAD-GRAPH:FILE$ type
   s" :" type e LOAD-GRAPH:EDGE-LINE@ FMT:.INT
   s" : row " type OWNER @ TEST:ITEM-NAME$ type
   e LOAD-GRAPH:EDGE-IMPORT? if s"  imports" else s"  launches" then type
   s"  registered entry " type row TEST:ITEM-NAME$ type
   s"  (" type to LOAD-GRAPH:FILE$ type s" )" type cr
   1 ERRORS +! ;

: CHECK-PRELOAD ( n -- ) {: f:n :}
   f OTHER {: row:n :}
   row 0 < if exit then
   s" entry guard: row " type OWNER @ TEST:ITEM-NAME$ type
   s"  preloads registered entry " type row TEST:ITEM-NAME$ type
   s"  (" type f LOAD-GRAPH:FILE$ type s" )" type cr
   1 ERRORS +! ;

: REACH ( n -- ) {: f:n :}
   f MARK @ WALK @ = if exit then
   WALK @ f MARK !
   f QTAIL @ QUEUE !
   QTAIL @ 1+ QTAIL ! ;

: FOLLOW ( n n -- ) {: src:n e:n :}
   e LOAD-GRAPH:EDGE-TO {: to:n :}
   to OTHER {: row:n :}
   to src <> row 0 >= and if src e row REPORT then
   to REACH ;

: EXPAND ( n -- ) {: src:n :}
   src LOAD-GRAPH:EDGE-FIRST {: first:n :}
   src LOAD-GRAPH:EDGE-COUNT 0 ?do src first i + FOLLOW loop ;

\ One walk per row: a file the row reached from an earlier file is not walked
\ again.
: CHECK-FILE ( n bool ptr u8 n -- ) {: row:n entry:bool a:ptr u:n :}
   row OWNER @ <> if row OWNER ! WALK @ 1+ WALK ! then
   a u LOAD-GRAPH:FILE-ID {: f:n :}
   entry 0= if f CHECK-PRELOAD then
   0 QHEAD ! 0 QTAIL !
   f REACH
   begin QHEAD @ QTAIL @ < while
      QHEAD @ QUEUE @ QHEAD @ 1+ QHEAD ! EXPAND
   repeat ;

public

\ Call it after LOAD-GRAPH:READ read the registry's graph.
: CHECK ( -- )
   RESET
   [: ENTRY-ADD ;] TEST:VISIT-LOAD-FILES
   [: CHECK-FILE ;] TEST:VISIT-LOAD-FILES
   ERRORS @ 0 > if E-SUITE-ROW throw then ;

;package
