require lib/string.f
require lib/test.f
require lib/test/outcome.f
require lib/test/subject.f
require tools/snap-heap-owner.f

package SNAP-HEAP-OWNER
public
variable OWNER-CELL
16 BUFFER: OWNER-BUFFER
: OWNER-USE ( -- ptr n ) OWNER-CELL ;
private

: OWNER-CASE ( -- )
   s" a created cell and buffer remain heap owners with compact DATA carriers" T-LABEL
   s" SNAP-HEAP-OWNER:OWNER-CELL" XREF-FIND CREATED? TTRUE
   s" SNAP-HEAP-OWNER:OWNER-BUFFER" XREF-FIND CREATED? TTRUE
   s" the shared decoder recovers the cell's actual address" T-LABEL
   s" SNAP-HEAP-OWNER:OWNER-CELL" XREF-FIND CHAIN-VALUE XREF-N>REC OWNER-CELL = TTRUE
   s" an ordinary word using the cell is not its owner" T-LABEL
   s" SNAP-HEAP-OWNER:OWNER-USE" XREF-FIND CREATED? TFALSE ;

\ Both maps are read as rows: DUMP's `<heap offset> <name>` and CODE-MAP's
\ `<code offset> <code length> <name>`, under header lines that start
\ `heap-owner`. A child runs the documented recipe and every other line it prints
\ must be one of the two rows; a number printed with `.` ends its line early.
$100000 constant MAP-CAP
$1000 constant MAP-ERR-CAP
20000 constant MAP-TIMEOUT-MS
MAP-CAP BUFFER: MAP-OUT
MAP-ERR-CAP BUFFER: MAP-ERR
variable HEAP-ROWS
variable CODE-ROWS
variable BAD-LINES

\ The name that ends a row: the rest of the line from `start`, one nonempty word.
: MAP-NAME? ( ptr u8 n n -- bool ) {: a:ptr u:n start:n :}
   start u >= if STR-FALSE exit then
   a start + u start - 32 COUNT-CHAR 0= ;

\ Whether the field at `start` is digits followed by a space, and where the
\ next field starts.
: MAP-FIELD ( ptr u8 n n -- n bool ) {: a:ptr u:n start:n :}
   a u 32 start SPLIT-NEXT {: f:ptr fu:n next:n more:bool :}
   next  more f fu STR-DIGITS? and ;

\ `want` digit fields, then the name.
: MAP-ROW? ( ptr u8 n n -- bool ) {: a:ptr u:n want:n :}
   0 STR-TRUE
   want 0 ?do
      if a u rot MAP-FIELD else STR-FALSE then
   loop
   if a u rot MAP-NAME? else drop STR-FALSE then ;

: MAP-COUNT+ ( ptr n -- ) {: cell:ptr :}
   cell @ 1 + cell ! ;

\ The first line that is neither, so a failure shows what was printed.
: MAP-BAD ( ptr u8 n -- ) {: a:ptr u:n :}
   BAD-LINES @ 0= if s" snap-heap-owner-test: bad map line [" type a u type s" ]" type cr then
   BAD-LINES MAP-COUNT+ ;

: MAP-LINE ( ptr u8 n -- ) {: a:ptr u:n :}
   a u s" heap-owner " STARTS-WITH? if exit then
   a u 1 MAP-ROW? if HEAP-ROWS MAP-COUNT+ exit then
   a u 2 MAP-ROW? if CODE-ROWS MAP-COUNT+ exit then
   a u MAP-BAD ;

: MAP-LINE-AT ( ptr u8 n n -- n ) {: a:ptr u:n start:n :}
   a u 10 start SPLIT-NEXT {: l:ptr lu:n next:n more:bool :}
   l lu MAP-LINE  next ;

\ The text ends with its last line's newline; split what comes before it.
: MAP-LINES ( ptr u8 n -- ) {: a:ptr u:n :}
   0 HEAP-ROWS !  0 CODE-ROWS !  0 BAD-LINES !
   0 begin dup u 1 - < while  a u 1 - rot MAP-LINE-AT  repeat drop ;

: MAP-CASE ( -- )
   s" the owner maps print one row per line" T-LABEL
   S\" require tools/snap-heap-owner.f\nSNAP-HEAP-OWNER:DUMP SNAP-HEAP-OWNER:CODE-MAP\n"
   MAP-OUT MAP-CAP >LEN MAP-ERR MAP-ERR-CAP >LEN MAP-TIMEOUT-MS >MS SUBJECT:RUN
   0 T-OUTCOME-EXITED= {: outu:len erru:len :}
   MAP-ERR erru LEN>N s" " T$=
   MAP-OUT outu LEN>N S\" \n" ENDS-WITH? TTRUE
   MAP-OUT outu LEN>N MAP-LINES
   BAD-LINES @ 0 T=
   HEAP-ROWS @ 0 > TTRUE
   CODE-ROWS @ 0 > TTRUE ;

: TESTS ( -- )
   T-RESET
   OWNER-CASE
   MAP-CASE
   T-REPORT ;
TESTS
;package
