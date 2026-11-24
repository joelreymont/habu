\ byte-edit.f - ordered replacement spans over arbitrary bytes (package EDIT).
\ The editor takes its storage from the allocator INIT takes and grows it per
\ edit; the source and borrowed replacements stay live until CLOSE. Edits are
\ sorted and nonoverlapping, insertions at one offset keep call order, and a
\ failed edit leaves the plan as it was. WRITE refuses a short or overlapping
\ destination before copying. Qualify EDIT:CLOSE and EDIT:WRITE (also global
\ tails). STORAGE CLASS. CALLER-OWNED: no module state.

require src/lib/prelude.f
require src/lib/error.f
require src/lib/allocator.f
require src/lib/vector.f

package EDIT
public
DEFLINEAR EDIT:editor
-9141 s" a destination too short for the edits, or a result longer than a cell holds: size the destination to the result"
   ERROR:CODE E-CAPACITY
-9142 s" a negative length, a null span with bytes, a span past the end of memory, or an edit outside the source" ERROR:CODE E-RANGE
-9143 s" an edit starts before the previous one ends: give edits in source order, without overlap" ERROR:CODE E-ORDER
-9144 s" source, a replacement or the destination share bytes: give each its own memory" ERROR:CODE E-ALIAS

private
STRUCTURE row DERIVE addr FIELD start n FIELD removed n FIELD replacement str ;STRUCTURE

STRUCTURE header DERIVE addr
   FIELD source str
   FIELD allocator ^ALLOC:allocator
   FIELD rows ^VEC:vector<row>
   FIELD last-end n
   FIELD result-len n
;STRUCTURE

$7FFFFFFFFFFFFFFF constant MAX-SIZE

\ Representation leaf: a span's address as a number, used only by the range
\ and alias checks; its extent and lifetime stay caller-owned.
: ADDRESS ( ^u8 -- n ) NULL-PTR BYTE-VIEW - ;

LINEAR: MINT ( ^header -- EDIT:editor )
LINEAR: ERASE ( EDIT:editor -- ^header )

: STATE ( EDIT:editor -- EDIT:editor ^header )
   ERASE dup MINT swap ;

: REQUIRE-SPAN ( str -- )
   {: source:str :}
   source STR:LEN 0 < if E-RANGE throw then
   source STR:DATA ADDRESS {: start:n :}
   start 0 < if E-RANGE throw then
   source STR:LEN 0 > start 0= and if E-RANGE throw then
   source STR:LEN MAX-SIZE start - > if E-RANGE throw then ;

: OVERLAPS? ( str str -- bool )
   {: first:str second:str :}
   first STR:LEN 0= second STR:LEN 0= or if false exit then
   first STR:DATA ADDRESS second STR:DATA ADDRESS second STR:LEN + <
   second STR:DATA ADDRESS first STR:DATA ADDRESS first STR:LEN + < and ;

: ROWS@ ( ^header -- ^VEC:vector<row> )
   HEADER-ROWS @ ;

: USED ( ^header -- n )
   ROWS@ VEC:COUNT ;

: ROW ( ^header n -- ^row )
   {: state index:n :}
   state ROWS@ index VEC:AT ;

: SOURCE$ ( ^header -- str )
   HEADER-SOURCE @ ;

public
: INIT ( ^ALLOC:allocator str -- EDIT:editor )
   {: al:^ source:str :}
   source REQUIRE-SPAN
   al [type] header ALLOC:NEW {: state :}
   [: al 0 [type] row VEC:NEW state HEADER-ROWS ! ;] catch {: code:n :}
   code 0<> if al state ALLOC:DELETE code throw then
   source state HEADER-SOURCE !
   al state HEADER-ALLOCATOR !
   source STR:LEN state HEADER-RESULT-LEN !
   state MINT ;

: CLOSE ( EDIT:editor -- )
   ERASE {: state :}
   state ROWS@ VEC:DELETE
   state HEADER-ALLOCATOR @ state ALLOC:DELETE ;

private
: EDIT-CHECK ( ^header n n str -- )
   {: state start:n removed:n replacement:str :}
   replacement REQUIRE-SPAN
   start 0 < removed 0 < or if E-RANGE throw then
   start state SOURCE$ STR:LEN > if E-RANGE throw then
   removed state SOURCE$ STR:LEN start - > if E-RANGE throw then
   start state HEADER-LAST-END @ < if E-ORDER throw then
   replacement STR:LEN MAX-SIZE state HEADER-RESULT-LEN @ removed - - > if
      E-CAPACITY throw
   then ;

\ PUSH grows the rows before any header cell changes, so a refused growth
\ leaves the plan as it was.
: SAVE-EDIT ( ^header n n str -- )
   {: state start:n removed:n replacement:str :}
   state ROWS@ VEC:PUSH {: row:^ :}
   start row ROW-START !
   removed row ROW-REMOVED !
   replacement row ROW-REPLACEMENT !
   start removed + state HEADER-LAST-END !
   state HEADER-RESULT-LEN @ removed - replacement STR:LEN + state HEADER-RESULT-LEN ! ;

: REPLACE-ARGS ( EDIT:editor off len str -- EDIT:editor ^header n n str )
   {: start:off removed:len replacement:str :}
   STATE start OFF>N removed LEN>N replacement ;

: APPLY-EDIT ( ^header n n str -- )
   {: state start:n removed:n replacement:str :}
   state start removed replacement EDIT-CHECK
   state start removed replacement SAVE-EDIT ;

public
\ Keep the editor below every throwing call. catch restores stack depth, so
\ binding the other arguments to locals here could overwrite its saved slot.
: REPLACE ( EDIT:editor off len str -- EDIT:editor )
   REPLACE-ARGS APPLY-EDIT ;

private
: DESTINATION-CHECK ( ^header str -- )
   {: state destination:str :}
   destination REQUIRE-SPAN
   state HEADER-RESULT-LEN @ destination STR:LEN > if E-CAPACITY throw then
   state SOURCE$ destination OVERLAPS? if E-ALIAS throw then
   state USED 0 ?do
      state i ROW ROW-REPLACEMENT @ destination OVERLAPS? if E-ALIAS throw then
   loop ;

: COPY-SPAN ( str ^u8 -- ^u8 )
   {: source:str destination :}
   source STR:DATA destination source STR:LEN >LEN BYTE-COPY-LEN
   destination source STR:LEN + ;

: COPY-EDIT ( ^header ^row n ^u8 -- n ^u8 )
   {: state row:^ prior:n destination :}
   state SOURCE$ STR:DATA prior + row ROW-START @ prior - >str
   destination COPY-SPAN
   {: next :}
   row ROW-START @ row ROW-REMOVED @ +
   row ROW-REPLACEMENT @ next COPY-SPAN ;

: WRITE-EDITS ( ^header ^u8 -- )
   {: state destination :}
   0 destination
   state USED 0 ?do
      {: prior:n next :}
      state state i ROW prior next COPY-EDIT
   loop
   {: prior:n next :}
   state SOURCE$ STR:DATA prior +
   state SOURCE$ STR:LEN prior - >str next COPY-SPAN drop ;

: WRITE-ARGS ( EDIT:editor ^u8 n -- EDIT:editor ^header ^u8 n )
   {: destination cap:n :}
   STATE destination cap ;

: WRITE-STATE ( ^header ^u8 n -- n )
   {: state destination cap:n :}
   state destination cap >str DESTINATION-CHECK
   state destination WRITE-EDITS
   state HEADER-RESULT-LEN @ ;

public
: WRITE ( EDIT:editor ^u8 n -- EDIT:editor n )
   WRITE-ARGS WRITE-STATE ;

seal-package
;package
