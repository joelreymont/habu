\ link-copy.f - the data a program reaches and the link's copy of the open
\ space: REACH-DATA marks the units and address rows a stripped program reads;
\ the copy is the space's bytes without its dead units, its kept rows listed in
\ cell order. A machine's writer gives each kept cell its image value.
\
\ THE DATA A STRIPPED PROGRAM REACHES.
\ A stripped program keeps the extents of the build's data it reaches: unit b,
\ b < BUILT-EXTS-N, is BUILT-EXTS row b; unit BUILT-EXTS-N the header data
\ [BASE, OPEN-DP); unit BUILT-EXTS-N + 1 the data no piece holds, the build
\ body's. A piece's extents tile its data but for the 1 to 7 bytes LAY aligns
\ by before its first laid text: no unit holds them and no address points in.
\ The header is live but for its string bodies, and a header address cell is
\ read once no piece stored it or a reached one did: a piece is reached once
\ one of its extents is live or one of its records' emissions is placed, and an
\ address a piece stored there is that piece's, as its object carries it. The
\ entry cell is read whatever stored it: it is the program's root. A read xt
\ cell roots the code it names and a read pointer cell makes its target's unit
\ live; each placed emission's data addresses make theirs live. An address
\ where one extent ends and the next starts may be either one's end or the
\ other's start, and makes both live; one where a piece ends and no piece
\ starts is the piece's. An address in an interned body keeps that body alone:
\ a header body lies in the header, any other in the piece that first interned
\ it, and every later piece spelling it shares it. The copy holds the dead
\ units and the header's bodies, all but the kept bodies, and every address
\ cell not read as zeros, and lists none of them. An engine keeps every unit
\ and cell.

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

: DATA-IMAGE-LEN ( -- n ) SPACE-DATA nip ;


\ A host address in the open space as its offset from the space's start.
: DATA-OFFSET ( n -- n )
   {: v:n :}
   v BASE @ - {: off:n :}
   off 0 >=  off SPACE-SIZE @ < and if off exit then
   s" link: a DATA pointer outside the build's space: " SAY v SAY-INT SAY-CR
   s" link: a DATA pointer outside the build's space" 74 die ;


DYNAMIC-BUFFER ROW-REACHED n         \ 1 once BUILT row r's piece is reached
DYNAMIC-BUFFER UNIT-LIVE n           \ 1 once the unit is live
DYNAMIC-BUFFER CELL-UNIT n           \ address row i's cell's unit
DYNAMIC-BUFFER CELL-READ n           \ 1 once address row i is read
DYNAMIC-BUFFER EM-READ n             \ 1 once emission e's data addresses are read
DYNAMIC-BUFFER BY-START n            \ the BUILT rows in data order
DYNAMIC-BUFFER BY-EXT n              \ the BUILT-EXTS rows in data order
variable CELLS-N                     \ the address rows UNITS saw
variable WENT-LIVE                   \ 1 once a unit or piece went live this round
variable KEPT-N                      \ the rows KEEP-LIST kept
DYNAMIC-BUFFER STR-AT n              \ the build's string bodies, ascending
DYNAMIC-BUFFER STR-KEPT n            \ 1 once body k is kept
variable STR-N

: HEADER-UNIT ( -- n ) BUILT-EXTS-N @ ;
: REST-UNIT ( -- n ) BUILT-EXTS-N @ 1 + ;
: REACHED? ( n -- bool ) UNIT-LIVE @ 0<> ;


\ BUILT row r's data span, the data pointer as its piece starts and ends.
: PIECE-SPAN ( n -- n n )
   BUILT-AT BUILT-DP + {: at:ptr :}
   at LE:U64@  at 8 + LE:U64@ ;


\ BUILT-EXTS row b's data, the address it starts at and its length.
: EXT-SPAN ( n -- n n ) {: b:n :}  b 4 BUILT-EXT @  b 5 BUILT-EXT @ ;

\ BUILT row row's extent holding the byte at v, -1 for none.
: EXT-HOLDING ( n n -- n )
   {: v:n row:n :}
   v row ROW-EXT-OF {: b:n :}
   b 0< if -1 exit then
   b EXT-SPAN {: start:n len:n :}
   v start len + < if b else -1 then ;

\ The unit holding the byte at data address v, -1 outside the space or in a
\ piece's alignment bytes.
: UNIT-OF ( n -- n )
   {: v:n :}
   v BASE @ - {: off:n :}
   off 0<  off SPACE-SIZE @ >= or if -1 exit then
   v OPEN-DP @ < if HEADER-UNIT exit then
   v DP-ROW {: row:n hit:bool :}
   hit if v row EXT-HOLDING else REST-UNIT then ;


: ROW-REACH ( n -- )
   {: row:n :}
   row ROW-REACHED @ 0<> if exit then
   1 row ROW-REACHED !  1 WENT-LIVE ! ;

: REACH-UNIT ( n -- )
   {: u:n :}
   u 0< if exit then
   u REACHED? if exit then
   1 u UNIT-LIVE !  1 WENT-LIVE !
   u BUILT-EXTS-N @ < if u 0 BUILT-EXT @ ROW-REACH then ;


\ Each extent of BUILT row row from b down that ends at v: those starting at v
\ hold no byte, and the first starting below v ends the walk.
: REACH-ENDS ( n n n -- )
   {: v:n row:n b:n :}
   row 2 * ROW-EXTS @ {: first:n :}
   b begin dup first >= while
      {: k:n :}
      k EXT-SPAN {: start:n len:n :}
      start len + v = if k REACH-UNIT then
      start v < if exit then
      k 1 -
   repeat
   drop ;

\ BUILT row row's last extent.
: LAST-EXT ( n -- n )
   {: row:n :}
   row 2 * ROW-EXTS @  row 2 * 1 + ROW-EXTS @ +  1 - ;


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


: REACH-ADDRESS ( n -- )
   {: v:n :}
   v STRING-OF {: k:n :}
   k 0 >= if 1 k STR-KEPT ! exit then
   v OPEN-DP @ < if v UNIT-OF REACH-UNIT exit then
   v DP-ROW {: row:n hit:bool :}
   hit if
      v row EXT-HOLDING REACH-UNIT
      v  row  v row ROW-EXT-OF  REACH-ENDS
      row PIECE-SPAN drop v <> if exit then
   then
   v DP-END-ROW {: end:n ended:bool :}
   ended if v end end LAST-EXT REACH-ENDS exit then
   hit 0= if v UNIT-OF REACH-UNIT then ;


\ Each address row's cell's unit; no unit live, no piece reached.
: UNITS ( -- )
   BUILT-N ROW-REACHED-RESERVE
   BUILT-N 0 ?do 0 i ROW-REACHED ! loop
   BUILT-EXTS-N @ 2 + {: units:n :}
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


: READ-ROW ( n -- )
   {: row:n :}
   row SCRATCH-ROW? if exit then
   row CELL-VALUE {: v:n :}
   v 0= if exit then
   row DATA-ROW? if v REACH-ADDRESS exit then
   v ROOT ;

\ Whether a reached piece stored into header cell off; whether any piece did.
: STORED-LIVE? ( n -- bool )
   {: off:n :}
   HSTORES-N @ 0 ?do
      i HSTORE-AT @ off = if
         i HSTORE-ROW @ ROW-REACHED @ 0<> if true unloop exit then
      then
   loop
   false ;

: STORED? ( n -- bool )
   {: off:n :}
   HSTORES-N @ 0 ?do i HSTORE-AT @ off = if true unloop exit then loop
   false ;


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


: READ-EM ( n -- )
   {: e:n :}
   e NSHADOW:ADDR-SITES 0 ?do
      e i NSHADOW:ADDR-SITE-KIND@ NEMIT:ADDR-DATA = if
         e i SITE-VALUE {: v:n has:bool :}
         has if v REACH-ADDRESS then
      then
   loop ;

\ The pieces holding emission e's records reached.
: EM-ROWS ( n -- )
   EM-FIRST @
   begin dup 0 >= while
      dup NSHADOW:RECORD@ REC-ROW@ if ROW-REACH else drop then
      REC-NEXT @
   repeat
   drop ;

: READ-EMS ( -- )
   NSHADOW:EMISSIONS 0 ?do
      i PLACED @ 0<>  i EM-READ @ 0= and if
         1 i EM-READ !  i READ-EM  i EM-ROWS
      then
   loop ;


\ The units the program reaches from the header. A round that makes no unit
\ live and reaches no piece has read every live unit's cells and every
\ emission placed so far.
: REACH-DATA ( -- )
   UNITS  STRINGS
   HEADER-UNIT REACH-UNIT
   begin
      0 WENT-LIVE !
      READ-CELLS  READ-EMS
      WENT-LIVE @ 0=
   until ;


: ROW-KEPT? ( n -- bool )
   {: i:n :}
   ENGINE? @ if true exit then
   i CELL-READ @ 0<> ;

: CELL-FITS ( n -- )
   {: off:n :}
   off CELL + DATA-IMAGE-LEN > if
      s" link: a declared cell past the build's data, at offset " SAY off SAY-INT SAY-CR
      s" link: a declared cell lies past the build's data" 74 die
   then ;

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


\ The data no piece holds zeroed: before, between and after the pieces.
: CUT-REST ( -- )
   BUILT-N {: n:n :}
   n BY-START-RESERVE
   n 0 ?do i i BY-START ! loop
   n 0 > if 0 BY-START n [: swap PIECE-SPAN drop swap PIECE-SPAN drop < ;] SORT:SORT! then
   OPEN-DP @ BASE @ -
   n 0 ?do
      {: at:n :}
      i BY-START @ PIECE-SPAN BASE @ - swap BASE @ - swap {: from:n to:n :}
      from at > if at from ZERO-COPY then
      at to max
   loop
   DATA-IMAGE-LEN ZERO-COPY ;

\ Each dead extent's data zeroed. An extent with no data, as one at another's
\ start is, holds no byte.
: CUT-EXTS ( -- )
   BUILT-EXTS-N @ {: n:n :}
   n BY-EXT-RESERVE
   n 0 ?do i i BY-EXT ! loop
   n 0 > if 0 BY-EXT n [: swap EXT-SPAN drop swap EXT-SPAN drop < ;] SORT:SORT! then
   OPEN-DP @ BASE @ -
   n 0 ?do
      {: at:n :}
      i BY-EXT @ {: b:n :}
      b EXT-SPAN {: start:n len:n :}
      start BASE @ - {: from:n :}
      len 0= if
         at
      else
         from at < if
            s" link: two extents of the build hold the data at offset " SAY from SAY-INT SAY-CR
            s" link: two extents of the build hold one byte" 74 die
         then
         b REACHED? 0= if from from len + ZERO-COPY then
         from len +
      then
   loop
   drop ;

\ Each body whose length cell lies in the header zeroed, over the span
\ KEEP-STRINGS copies back.
: CUT-HEADER-STRINGS ( -- )
   STR-N @ 0 ?do
      i STR-AT @ {: b:n :}
      b CELL - {: at:n :}
      at BASE @ -  {: from:n :}
      from 0 >=  at OPEN-DP @ <  and if
         from  from CELL + b BODY-U +  ZERO-COPY
      then
   loop ;

\ A stripped program's copy without its dead units. A piece's alignment bytes,
\ which no unit holds, keep theirs.
: CUT-DEAD ( -- )
   REST-UNIT REACHED? 0= if CUT-REST then
   CUT-EXTS  CUT-HEADER-STRINGS ;


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


\ The copy's rows in cell order, not the build's mark order: the target's first
\ MARK rebuilds the index HEADER-CELLS zeroes.
: SORT-ROWS ( -- )
   LIST-AT {: at:n :}
   SNAP-RELOC:XTCELL-N-CELL COPY-AT LE:U64@ {: n:n :}
   n 0 > if at COPY-CELLS n [: ROW-OFF swap ROW-OFF swap < ;] SORT:SORT! then ;


: KEPT-COPY ( -- )
   FILL-COPY
   ENGINE? @ 0= if CUT-DEAD KEEP-STRINGS KEEP-LIST then
   SORT-ROWS ;

;package
