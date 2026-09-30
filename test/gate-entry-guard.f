\ Gate entry files are execution roots. A row may share an inert helper, but
\ importing or launching another row's entry silently runs that suite twice.
\
\ The guard walks the load graph test/gate-images.f derived, from every file a
\ row loads through every import and launch. A load of another row's entry is
\ refused, naming the file and line that makes it, and so is a preload that is
\ another row's entry. A file may name itself, and rows may share an entry: a
\ tier twin runs one file at both tiers.

require lib/errors.f
require lib/fmt.f
require lib/test.f
require test/gate-images.f

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
   GATE-IMAGES:FILE-COUNT {: n:n :}
   n ROW-A-RESERVE n ROW-B-RESERVE n MARK-RESERVE n QUEUE-RESERVE
   n 0 ?do -1 i ROW-A ! -1 i ROW-B ! 0 i MARK ! loop
   0 WALK ! -1 OWNER ! 0 ERRORS ! ;

\ Every registered file must be one DERIVE read. A file keeps the first row that
\ has it as an entry and one other.
: ENTRY-ADD ( n bool ptr u8 n -- ) {: row:n entry:bool a:ptr u:n :}
   a u GATE-IMAGES:FILE-ID {: f:n :}
   entry 0= if exit then
   f ROW-A @ 0 < if row f ROW-A ! exit then
   f ROW-A @ row <> if row f ROW-B ! then ;

\ A row other than the owner that has the file as an entry, else -1.
: OTHER ( n -- n ) {: f:n :}
   f ROW-A @ OWNER @ <> if f ROW-A @ exit then
   f ROW-B @ ;

: REPORT ( n n n -- ) {: src:n e:n row:n :}
   e GATE-IMAGES:EDGE-TO {: to:n :}
   s" entry guard: " type src GATE-IMAGES:FILE$ type
   s" :" type e GATE-IMAGES:EDGE-LINE@ FMT:.INT
   s" : row " type OWNER @ TEST:ITEM-NAME$ type
   e GATE-IMAGES:EDGE-IMPORT? if s"  imports" else s"  launches" then type
   s"  registered entry " type row TEST:ITEM-NAME$ type
   s"  (" type to GATE-IMAGES:FILE$ type s" )" type cr
   1 ERRORS +! ;

: CHECK-PRELOAD ( n -- ) {: f:n :}
   f OTHER {: row:n :}
   row 0 < if exit then
   s" entry guard: row " type OWNER @ TEST:ITEM-NAME$ type
   s"  preloads registered entry " type row TEST:ITEM-NAME$ type
   s"  (" type f GATE-IMAGES:FILE$ type s" )" type cr
   1 ERRORS +! ;

: REACH ( n -- ) {: f:n :}
   f MARK @ WALK @ = if exit then
   WALK @ f MARK !
   f QTAIL @ QUEUE !
   QTAIL @ 1+ QTAIL ! ;

: FOLLOW ( n n -- ) {: src:n e:n :}
   e GATE-IMAGES:EDGE-TO {: to:n :}
   to OTHER {: row:n :}
   to src <> row 0 >= and if src e row REPORT then
   to REACH ;

: EXPAND ( n -- ) {: src:n :}
   src GATE-IMAGES:EDGE-FIRST {: first:n :}
   src GATE-IMAGES:EDGE-COUNT 0 ?do src first i + FOLLOW loop ;

\ One walk per row: a file the row reached from an earlier file is not walked
\ again.
: CHECK-FILE ( n bool ptr u8 n -- ) {: row:n entry:bool a:ptr u:n :}
   row OWNER @ <> if row OWNER ! WALK @ 1+ WALK ! then
   a u GATE-IMAGES:FILE-ID {: f:n :}
   entry 0= if f CHECK-PRELOAD then
   0 QHEAD ! 0 QTAIL !
   f REACH
   begin QHEAD @ QTAIL @ < while
      QHEAD @ QUEUE @ QHEAD @ 1+ QHEAD ! EXPAND
   repeat ;

public

\ Refuse a registry whose rows run another row's entry. Call it after
\ GATE-IMAGES:DERIVE read the registry's graph.
: CHECK ( -- )
   RESET
   [: ENTRY-ADD ;] TEST:VISIT-LOAD-FILES
   [: CHECK-FILE ;] TEST:VISIT-LOAD-FILES
   ERRORS @ 0 > if E-SUITE-ROW throw then ;

;package
