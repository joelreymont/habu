\ Ordered edits borrow source/replacement spans until CLOSE. WRITE validates
\ the complete destination before copying; the source is never modified.
require lib/prelude.f

package EDIT
public
DEFLINEAR EDIT:editor
-9140 constant E-STORAGE
-9141 constant E-CAPACITY
-9142 constant E-RANGE
-9143 constant E-ORDER
-9144 constant E-ALIAS

private
7 constant HEADER-CELLS
4 constant ROW-CELLS
0 constant SOURCE-IDX
1 cells constant SOURCE-LEN
2 cells constant CAPACITY
3 cells constant USED
4 cells constant LAST-END
5 cells constant RESULT-LEN
6 cells constant STORAGE-LEN
$7FFFFFFFFFFFFFFF constant MAX-SIZE

\ Representation leaves only; extent/lifetime ownership remains caller-owned.
\ Retirement owner: cap:raw-pointer-lifetime. Tested by byte-edit-test.f.
TRUSTED: ADDRESS ( ptr u8 -- n ) ;
TRUSTED: MINT ( ptr n -- EDIT:editor ) ;
TRUSTED: STATE ( EDIT:editor -- EDIT:editor ptr n ) dup ;
TRUSTED: CONSUME ( EDIT:editor -- ) drop ;

: REQUIRE-SPAN ( ptr u8 n -- )
   {: source:ptr size:n :}
   size 0 < if E-RANGE throw then
   source ADDRESS {: start:n :}
   start 0 < if E-RANGE throw then
   size 0 > start 0= and if E-RANGE throw then
   size MAX-SIZE start - > if E-RANGE throw then ;

: OVERLAPS? ( ptr u8 n ptr u8 n -- bool )
   {: first:ptr first-len:n second:ptr second-len:n :}
   first-len 0= second-len 0= or if false exit then
   first ADDRESS second ADDRESS second-len + <
   second ADDRESS first ADDRESS first-len + < and ;

: ROW ( ptr n n -- ptr n )
   {: state index:n :}
   state HEADER-CELLS index ROW-CELLS * + cells + ;

: SOURCE$ ( ptr n -- ptr u8 n )
   {: state :}
   state SOURCE-IDX ptr-field @ state SOURCE-LEN + @ ;

: ROW-SOURCE ( ptr n -- ptr u8 n )
   {: row :}
   row 2 ptr-field @ row 3 cells + @ ;

: SIZE-CHECK ( n -- )
   dup 0 < if E-CAPACITY throw then
   MAX-SIZE HEADER-CELLS cells - ROW-CELLS cells / > if
      E-CAPACITY throw
   then ;

public
: STORAGE-BYTES ( n -- n )
   dup SIZE-CHECK
   ROW-CELLS * HEADER-CELLS + cells ;

private
: STORAGE-CHECK ( ptr n n ptr u8 n -- )
   {: storage cap:n source size:n :}
   storage BYTE-VIEW cap REQUIRE-SPAN
   storage BYTE-VIEW ADDRESS CELL 1- and 0<> if E-STORAGE throw then
   cap 0 STORAGE-BYTES < if E-CAPACITY throw then
   source size REQUIRE-SPAN
   storage BYTE-VIEW cap source size OVERLAPS? if E-ALIAS throw then ;

: INIT-STATE ( ptr n n ptr u8 n -- )
   {: storage cap:n source size:n :}
   source storage SOURCE-IDX ptr-field !
   size storage SOURCE-LEN + !
   cap HEADER-CELLS cells - ROW-CELLS cells / storage CAPACITY + !
   0 storage USED + !
   0 storage LAST-END + !
   size storage RESULT-LEN + !
   cap storage STORAGE-LEN + ! ;

public
: INIT ( ptr n n ptr u8 n -- EDIT:editor )
   {: storage cap:n source size:n :}
   storage cap source size STORAGE-CHECK
   storage cap source size INIT-STATE
   storage MINT ;

: CLOSE ( EDIT:editor -- )
   CONSUME ;

private
: EDIT-CHECK ( ptr n n n ptr u8 n -- )
   {: state start:n removed:n replacement added:n :}
   replacement added REQUIRE-SPAN
   start 0 < removed 0 < or if E-RANGE throw then
   start state SOURCE-LEN + @ > if E-RANGE throw then
   removed state SOURCE-LEN + @ start - > if E-RANGE throw then
   start state LAST-END + @ < if E-ORDER throw then
   state USED + @ state CAPACITY + @ >= if E-CAPACITY throw then
   added MAX-SIZE state RESULT-LEN + @ removed - - > if
      E-CAPACITY throw
   then
   state BYTE-VIEW state STORAGE-LEN + @ replacement added OVERLAPS? if
      E-ALIAS throw
   then ;

: SAVE-EDIT ( ptr n n n ptr u8 n -- )
   {: state start:n removed:n replacement added:n :}
   state state USED + @ ROW {: row :}
   start row !
   removed row CELL + !
   replacement row 2 ptr-field !
   added row 3 cells + !
   start removed + state LAST-END + !
   state RESULT-LEN + @ removed - added + state RESULT-LEN + !
   1 state USED + +! ;

public
: REPLACE ( EDIT:editor off len ptr u8 n -- EDIT:editor )
   {: start:off removed:len replacement added:n :}
   STATE {: state :}
   state start OFF>N removed LEN>N replacement added EDIT-CHECK
   state start OFF>N removed LEN>N replacement added SAVE-EDIT ;

private
: DESTINATION-CHECK ( ptr n ptr u8 n -- )
   {: state destination cap:n :}
   destination cap REQUIRE-SPAN
   state RESULT-LEN + @ cap > if E-CAPACITY throw then
   state SOURCE$ destination cap OVERLAPS? if E-ALIAS throw then
   state BYTE-VIEW state STORAGE-LEN + @ destination cap OVERLAPS? if
      E-ALIAS throw
   then
   state USED + @ 0 ?do
      state i ROW ROW-SOURCE destination cap OVERLAPS? if E-ALIAS throw then
   loop ;

: COPY-SPAN ( ptr u8 n ptr u8 -- ptr u8 )
   {: source size:n destination :}
   source destination size >LEN BYTE-COPY-LEN
   destination size + ;

: COPY-EDIT ( ptr n ptr n n ptr u8 -- n ptr u8 )
   {: state row prior:n destination :}
   state SOURCE-IDX ptr-field @ prior + row @ prior - destination COPY-SPAN
   {: next :}
   row @ row CELL + @ +
   row ROW-SOURCE next COPY-SPAN ;

: WRITE-EDITS ( ptr n ptr u8 -- )
   {: state destination :}
   0 destination
   state USED + @ 0 ?do
      {: prior:n next :}
      state state i ROW prior next COPY-EDIT
   loop
   {: prior:n next :}
   state SOURCE-IDX ptr-field @ prior +
   state SOURCE-LEN + @ prior - next COPY-SPAN drop ;

public
: WRITE ( EDIT:editor ptr u8 n -- EDIT:editor n )
   {: destination cap:n :}
   STATE {: state :}
   state destination cap DESTINATION-CHECK
   state destination WRITE-EDITS
   state RESULT-LEN + @ ;

private
get-current prot-wid-add
public
get-current prot-wid-add
;package
