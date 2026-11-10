\ link-copy.f - the data a program reaches and the link's copy of the open
\ space: REACH-DATA marks the units and address rows a stripped program reads;
\ the copy is the space's bytes without its dead units, its kept rows listed in
\ cell order. A machine's writer gives each kept cell its image value. Words
\ visible only inside LINKER.

require src/lib/le.f
require src/lib/sort.f
require src/habu/layout.f
require src/habu/address-cells.f
require src/habu/intern.f
require src/compiler/native/emission.f
require src/compiler/native/shadow.f
require src/habu/linker.f
require src/habu/link-band.f

package LINKER
private

\ The copy's length is the open space's.
: DATA-IMAGE-LEN ( -- n ) SPACE-DATA nip ;


\ A host address in the open space as its offset from the space's start.
: DATA-OFFSET ( n -- n )
   {: v:n :}
   v BASE @ - {: off:n :}
   off 0 >=  off SPACE-SIZE @ < and if off exit then
   s" link: a DATA pointer outside the build's space: " SAY v SAY-INT SAY-CR
   s" link: a DATA pointer outside the build's space" 74 die ;


\ Each row of the space's address cells, handed to visit.
: EACH-ROW ( [ n -- ] -- )
   {: visit :}
   ROWS {: rows:ptr count:n :}
   count 0 ?do rows i cells + @ visit execute loop ;


\ ---- the data a stripped program reaches ------------------------------------------
\ A stripped program keeps the data of the files it reaches, whole files: unit r,
\ r < BUILT-N, is the file whose first BUILT row is r, each of its pieces; unit
\ BUILT-N the header data [BASE, OPEN-DP); unit BUILT-N + 1 the data no piece
\ holds, the build body's and the facts texts. The header is live, and a header
\ address cell is read once no file stored it or a live one did: an address a
\ file stored there is that file's, as its object carries it. The entry cell is
\ read whatever stored it: it is the program's root. A read xt cell roots the
\ code it names and a read pointer cell makes its target's unit live;
\ each placed emission's data addresses and defer trailer make theirs live.
\ An address in a string literal's body keeps that body alone: the body lies in
\ the file that first interned it, and every later file spelling it shares it
\ (src/habu/intern-table.f). The copy holds the dead units, all but their
\ kept bodies, and every address cell not read as zeros, and lists none of
\ them. An engine keeps every unit and cell.

DYNAMIC-BUFFER ROW-UNIT n            \ BUILT row r's unit
DYNAMIC-BUFFER UNIT-LIVE n           \ 1 once the unit is live
DYNAMIC-BUFFER CELL-UNIT n           \ address row i's cell's unit
DYNAMIC-BUFFER CELL-READ n           \ 1 once address row i is read
DYNAMIC-BUFFER EM-READ n             \ 1 once emission e's data addresses are read
DYNAMIC-BUFFER BY-START n            \ the BUILT rows in data order
variable CELLS-N                     \ the address rows UNITS saw
variable WENT-LIVE                   \ 1 once a unit went live this round
variable KEPT-N                      \ the rows KEEP-LIST kept
DYNAMIC-BUFFER STR-AT n              \ the build's string bodies, ascending
DYNAMIC-BUFFER STR-KEPT n            \ 1 once body k is kept
variable STR-N

: HEADER-UNIT ( -- n ) BUILT-N ;
: REST-UNIT ( -- n ) BUILT-N 1 + ;
: REACHED? ( n -- bool ) UNIT-LIVE @ 0<> ;


\ BUILT row r's data span, the data pointer as its piece starts and ends.
: PIECE-SPAN ( n -- n n )
   BUILT-AT BUILT-DP + {: at:ptr :}
   at LE:U64@  at 8 + LE:U64@ ;


\ The first BUILT row with row r's file key: every piece of a file names one unit.
: FIRST-PIECE ( n -- n )
   {: r:n :}
   r 0 ?do
      i BUILT-AT KEY-BYTES  r BUILT-AT KEY-BYTES  STR= if i unloop exit then
   loop
   r ;


\ The unit holding data address v, -1 outside the space. An address a piece ends
\ at is that piece's when no piece starts there, as DATA-REF+ names it.
: UNIT-OF ( n -- n )
   {: v:n :}
   v BASE @ - {: off:n :}
   off 0<  off SPACE-SIZE @ >= or if -1 exit then
   v OPEN-DP @ < if HEADER-UNIT exit then
   v DP-ROW {: row:n hit:bool :}
   hit if row ROW-UNIT @ exit then
   v DP-END-ROW {: end-row:n end-hit:bool :}
   end-hit if end-row ROW-UNIT @ exit then
   REST-UNIT ;


: REACH-UNIT ( n -- )
   {: u:n :}
   u 0< if exit then
   u REACHED? if exit then
   1 u UNIT-LIVE !  1 WENT-LIVE ! ;


: STR+ ( n -- )
   {: b:n :}
   STR-N @ 1 + STR-AT-RESERVE
   b STR-N @ STR-AT !  1 STR-N +! ;

: STRINGS ( -- )
   0 STR-N !  ['] STR+ INTERN:EACH-STRING
   STR-N @ {: n:n :}
   n 0 > if 0 STR-AT n [: < ;] SORT:SORT! then
   n STR-KEPT-RESERVE
   n 0 ?do 0 i STR-KEPT ! loop ;


\ The body whose bytes [b, b + its length) hold data address v, -1 for none; an
\ empty body holds the byte LAY gives it.
: STRING-OF ( n -- n )
   {: v:n :}
   0 STR-N @
   begin 2dup < while
      2dup + 1 rshift {: mid:n :}
      mid STR-AT @ v <= if nip mid 1 + swap else drop mid then
   repeat
   drop 1 - {: k:n :}
   k 0< if -1 exit then
   k STR-AT @ {: b:n :}
   v  b  b BODY-U 1 max +  < if k exit then
   -1 ;


\ Data address v: the string body holding it is kept, else its unit goes live.
: REACH-ADDRESS ( n -- )
   {: v:n :}
   v STRING-OF {: k:n :}
   k 0 >= if 1 k STR-KEPT ! exit then
   v UNIT-OF REACH-UNIT ;


\ Each BUILT row's unit, each address row's cell's unit; no unit live.
: UNITS ( -- )
   BUILT-N ROW-UNIT-RESERVE
   BUILT-N 0 ?do i FIRST-PIECE i ROW-UNIT ! loop
   BUILT-N 2 + {: units:n :}
   units UNIT-LIVE-RESERVE
   units 0 ?do 0 i UNIT-LIVE ! loop
   ROWS {: rows:ptr count:n :}
   count CELLS-N !
   count CELL-UNIT-RESERVE  count CELL-READ-RESERVE
   count 0 ?do
      rows i cells + @ ROW-OFF BASE @ + UNIT-OF  i CELL-UNIT !
      0 i CELL-READ !
   loop
   NSHADOW:EMISSIONS EM-READ-RESERVE
   NSHADOW:EMISSIONS 0 ?do 0 i EM-READ ! loop ;


\ A read address row: an xt cell roots the code it names, a pointer cell makes
\ its target's unit live.
: READ-ROW ( n -- )
   {: row:n :}
   row SCRATCH-ROW? if exit then
   row CELL-VALUE {: v:n :}
   v 0= if exit then
   row DATA-ROW? if v REACH-ADDRESS exit then
   v ROOT ;

\ Whether a piece of a live file stored into header cell off; whether any piece did.
: STORED-LIVE? ( n -- bool )
   {: off:n :}
   HSTORES-N @ 0 ?do
      i HSTORE-AT @ off = if
         i HSTORE-ROW @ ROW-UNIT @ REACHED? if true unloop exit then
      then
   loop
   false ;

: STORED? ( n -- bool )
   {: off:n :}
   HSTORES-N @ 0 ?do i HSTORE-AT @ off = if true unloop exit then loop
   false ;


\ Address row i, row, is read once its unit is live and, for a header cell files
\ stored into other than the entry, one of those files is.
: ROW-LIVE? ( n n -- bool )
   {: i:n row:n :}
   i CELL-UNIT @ {: u:n :}
   u REACHED? 0= if false exit then
   u HEADER-UNIT <> if true exit then
   row ROW-OFF {: off:n :}
   off APP-ENTRY:XT-CELL = if true exit then
   off STORED? 0= if true exit then
   off STORED-LIVE? ;

: READ-CELLS ( -- )
   ROWS {: rows:ptr count:n :}
   count CELLS-N @ <> if E-COMP-STATE throw then
   count 0 ?do
      rows i cells + @ {: row:n :}
      i CELL-READ @ 0= if
         i row ROW-LIVE? if  1 i CELL-READ !  row READ-ROW  then
      then
   loop ;


\ Placed emission e's data addresses and defer trailer make their units live.
: READ-EM ( n -- )
   {: e:n :}
   e NSHADOW:ADDR-SITES 0 ?do
      e i NSHADOW:ADDR-SITE-KIND@ NEMIT:ADDR-DATA = if
         e i SITE-VALUE {: v:n has:bool :}
         has if v REACH-ADDRESS then
      then
   loop
   e EM-TRAILER {: cell:n :}
   cell 0<> if cell UNIT-OF REACH-UNIT then ;

: READ-EMS ( -- )
   NSHADOW:EMISSIONS 0 ?do
      i PLACED @ 0<>  i EM-READ @ 0= and if 1 i EM-READ !  i READ-EM then
   loop ;


\ The units the program reaches from the header. A round that makes no unit
\ live has read every live unit's cells and every emission placed so far.
: REACH-DATA ( -- )
   UNITS  STRINGS
   HEADER-UNIT REACH-UNIT
   begin
      0 WENT-LIVE !
      READ-CELLS  READ-EMS
      WENT-LIVE @ 0=
   until ;


\ Address row i's cell is copied: every row of an engine, a stripped program's
\ rows REACH-DATA read.
: ROW-KEPT? ( n -- bool )
   {: i:n :}
   ENGINE? @ if true exit then
   i CELL-READ @ 0<> ;

\ Each kept address row, handed to visit.
: EACH-KEPT ( [ n -- ] -- )
   {: visit :}
   ROWS {: rows:ptr count:n :}
   count 0 ?do i ROW-KEPT? if rows i cells + @ visit execute then loop ;


: COPY-N ( -- n ) DATA-IMAGE-LEN CELL 1- + CELL / ;
: COPY-AT ( n -- ptr u8 ) 0 COPY-CELLS BYTE-VIEW swap + ;

: FILL-COPY ( -- )
   COPY-N {: n:n :}
   n COPY-CELLS-RESERVE
   0  n 1- COPY-CELLS !
   SPACE-DATA {: p:ptr u:n :}
   p 0 COPY-AT u BYTE-COPY ;


\ The copy's bytes [from, to), as offsets from the space, zeroed.
: ZERO-COPY ( n n -- )
   {: from:n to:n :}
   to from ?do 0 i COPY-AT c! loop ;


\ A stripped program's copy without its dead units: each dead file's pieces
\ zeroed, and the data before, between and after the pieces when the rest is dead.
\ A piece with no data, as the open's records are, holds no byte.
: CUT-DEAD ( -- )
   BUILT-N {: n:n :}
   n BY-START-RESERVE
   n 0 ?do i i BY-START ! loop
   n 0 > if 0 BY-START n [: swap PIECE-SPAN drop swap PIECE-SPAN drop < ;] SORT:SORT! then
   REST-UNIT REACHED? 0= {: rest-dead:bool :}
   OPEN-DP @ BASE @ -
   n 0 ?do
      {: at:n :}
      i BY-START @ {: r:n :}
      r PIECE-SPAN BASE @ - swap BASE @ - swap {: from:n to:n :}
      from to = if
         at
      else
         from at < if
            s" link: two pieces of the build hold the data at offset " SAY from SAY-INT SAY-CR
            s" link: two pieces of the build hold one byte" 74 die
         then
         rest-dead if at from ZERO-COPY then
         r ROW-UNIT @ REACHED? 0= if from to ZERO-COPY then
         to
      then
   loop
   {: last:n :}
   rest-dead if last DATA-IMAGE-LEN ZERO-COPY then ;


\ Each kept body's bytes and length cell back in the copy, after CUT-DEAD.
: KEEP-STRINGS ( -- )
   SPACE-DATA drop {: p:ptr :}
   STR-N @ 0 ?do
      i STR-KEPT @ 0<> if
         i STR-AT @ {: b:n :}
         b CELL - BASE @ - {: from:n :}
         p from +  from COPY-AT  CELL b BODY-U +  BYTE-COPY
      then
   loop ;


\ The rows list's cell index in the copy, as the header's base field names it.
: LIST-AT ( -- n )
   SNAP-RELOC:XTCELL-N-CELL ADDRESS-CELLS:BASE-FIELD +  SPACE-AT @ CELL / ;

\ A stripped program's copy lists only its kept rows, after CUT-DEAD, which may
\ have zeroed the list; a dropped row's cell and its list slot hold 0.
: KEEP-LIST ( -- )
   0 KEPT-N !
   ROWS {: rows:ptr count:n :}
   count 0 ?do
      rows i cells + @ {: row:n :}
      i ROW-KEPT? if
         row  LIST-AT KEPT-N @ +  COPY-CELLS !  1 KEPT-N +!
      else
         0 row ROW-OFF COPY-AT LE:U64!
      then
   loop
   count KEPT-N @ ?do 0  LIST-AT i + COPY-CELLS ! loop
   KEPT-N @ SNAP-RELOC:XTCELL-N-CELL COPY-AT LE:U64! ;


\ The copy's rows in cell order, not the build's mark order: the product's first
\ MARK rebuilds the index HEADER-CELLS zeroes.
: SORT-ROWS ( -- )
   LIST-AT {: at:n :}
   SNAP-RELOC:XTCELL-N-CELL COPY-AT LE:U64@ {: n:n :}
   n 0 > if at COPY-CELLS n [: ROW-OFF swap ROW-OFF swap < ;] SORT:SORT! then ;

;package
