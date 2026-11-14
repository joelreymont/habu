\ link-copy.f - the data a program reaches and the link's copy of the open
\ space: REACH-DATA marks the units and address rows a stripped program reads
\ and lays its data out; the copy is that data, its kept rows listed in cell
\ order. A machine's writer gives each kept cell its image value.
\
\ THE DATA A STRIPPED PROGRAM REACHES.
\ A stripped program keeps the extents of the build's data it reaches: unit b,
\ b < BUILT-EXTS-N, is BUILT-EXTS row b; unit BUILT-EXTS-N the header data
\ [BASE, OPEN-DP); unit BUILT-EXTS-N + 1 the data no piece holds, the build
\ body's. A piece's extents tile its data but for the 1 to 8 bytes LAY aligns
\ by before its first laid text: no unit holds them and no address points in.
\ The header is live but for its string bodies, and a header address cell is
\ read once no piece stored it or a reached one did: a piece is reached once
\ one of its extents is live or one of its records' emissions is placed, and an
\ address a piece stored there is that piece's, as its object carries it. The
\ entry cell is read whatever stored it: it is the program's root. A read xt
\ cell roots the code it names and a read pointer cell makes its target's unit
\ live; each placed emission's data addresses make theirs live. An address
\ where data ends lies in its extent, since no extent or piece starts where
\ the data before it ends; one where a piece ends makes the piece's last
\ extent live and the unit holding it. An address in an interned body or its
\ length cell keeps that body alone: a header body lies in the header, any
\ other in the piece that first interned it, and every later piece spelling it
\ shares it.
\
\ THE IMAGE'S DATA. A stripped program's is the header as the build left it,
\ its bodies the program does not keep zeroed; then the live units in build
\ order, each laid at the first offset past the one before that keeps its
\ offset modulo a cell, a monotone map, so an interior or end address keeps its
\ meaning; then the kept bodies whose unit is dead, sorted by length then
\ bytes, each from a cell boundary with its length cell and the byte past it.
\ The alignment bytes no unit holds are not laid. An address cell not read
\ holds zeros where it is laid, and the list names none of them. An engine's
\ is the space as is.

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
\ LAY-DATA's segments, in build order: build start from NULL-PTR, length,
\ image offset.
3 constant SEG-CELLS
DYNAMIC-BUFFER SEGS n
variable SEG-N
DYNAMIC-BUFFER LAID-AT n             \ body k's image offset, 0 for a body not laid
DYNAMIC-BUFFER BY-BODY n             \ the laid bodies in laid order
variable LAID-END                    \ where LAY-DATA lays next; then the laid length
variable EXT-I                       \ BY-EXT's next row to lay

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


\ The body whose bytes [b - CELL, b + its length], the byte past its text
\ included, hold data address v; -1 for none.
: STRING-OF ( n -- n )
   {: v:n :}
   0 STR-N @
   begin 2dup < while
      2dup + 1 rshift {: mid:n :}
      mid STR-AT @ CELL - v <= if nip mid 1 + swap else drop mid then
   repeat
   drop 1 - {: k:n :}
   k 0< if -1 exit then
   k STR-AT @ {: b:n :}
   v  b  b BODY-U 1 + +  < if k exit then
   -1 ;

\ Whether body j sorts before body k: the shorter first, then by the first
\ byte that differs.
: BODY< ( n n -- bool )
   {: j:n k:n :}
   j STR-AT @ {: a:n :}
   k STR-AT @ {: b:n :}
   a BODY-U {: au:n :}
   b BODY-U {: bu:n :}
   au bu <> if au bu < exit then
   NULL-PTR BYTE-VIEW a + {: p:ptr :}
   NULL-PTR BYTE-VIEW b + {: q:ptr :}
   au 0 ?do
      p i + c@  q i + c@  2dup <> if < unloop exit then  2drop
   loop
   false ;


: REACH-ADDRESS ( n -- )
   {: v:n :}
   v STRING-OF {: k:n :}
   k 0 >= if 1 k STR-KEPT ! exit then
   v DP-END-ROW {: end:n ended:bool :}
   ended if end LAST-EXT REACH-UNIT then
   v UNIT-OF REACH-UNIT ;


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


\ The build's DP, where the space's data ends.
: SPACE-END ( -- n ) SPACE-DATA nip BASE @ + ;

: SEG ( n n -- ptr n ) swap SEG-CELLS * + SEGS ;

\ The len bytes at build address start, laid at the first offset from LAID-END
\ that keeps their offset modulo a cell.
: SEG+ ( n n -- )
   {: start:n len:n :}
   len 0= if exit then
   LAID-END @ {: at:n :}
   start BASE @ -  at -  CELL 1- and  at + {: img:n :}
   SEG-N @ {: s:n :}
   s 1 + SEG-CELLS * SEGS-RESERVE
   start s 0 SEG !  len s 1 SEG !  img s 2 SEG !
   1 SEG-N +!
   img len + LAID-END ! ;

\ BUILT-EXTS row b, laid when live.
: LAY-EXT ( n -- )
   {: b:n :}
   b REACHED? if b EXT-SPAN SEG+ then ;

\ Each extent not yet laid that starts below v, in data order.
: EXTS-BELOW ( n -- )
   {: v:n :}
   begin
      EXT-I @ BUILT-EXTS-N @ < if EXT-I @ BY-EXT @ EXT-SPAN drop v < else false then
   while
      EXT-I @ BY-EXT @ LAY-EXT  1 EXT-I +!
   repeat ;

: SORT-EXTS ( -- )
   BUILT-EXTS-N @ {: n:n :}
   n BY-EXT-RESERVE
   n 0 ?do i i BY-EXT ! loop
   n 0 > if 0 BY-EXT n [: swap EXT-SPAN drop swap EXT-SPAN drop < ;] SORT:SORT! then ;

: SORT-PIECES ( -- )
   BUILT-N {: n:n :}
   n BY-START-RESERVE
   n 0 ?do i i BY-START ! loop
   n 0 > if 0 BY-START n [: swap PIECE-SPAN drop swap PIECE-SPAN drop < ;] SORT:SORT! then ;

\ The live units from OPEN-DP in data order: the extents, and when the rest
\ unit is live each span no piece holds, before, between and after the pieces.
: LAY-UNITS ( -- )
   SORT-EXTS  SORT-PIECES
   0 SEG-N !  0 EXT-I !
   REST-UNIT REACHED? {: rest:bool :}
   OPEN-DP @
   BUILT-N 0 ?do
      {: at:n :}
      i BY-START @ PIECE-SPAN {: from:n to:n :}
      at EXTS-BELOW
      rest  from at >  and if at  from at -  SEG+ then
      at to max
   loop
   {: last:n :}
   last EXTS-BELOW
   rest  SPACE-END last >  and if last  SPACE-END last -  SEG+ then ;


\ Whether body k is laid after the units: kept, its length cell past the
\ header and its unit dead.
: LAID? ( n -- bool )
   {: k:n :}
   k STR-KEPT @ 0= if false exit then
   k STR-AT @ CELL - {: at:n :}
   at OPEN-DP @ < if false exit then
   at UNIT-OF {: u:n :}
   u 0< if true exit then
   u REACHED? 0= ;

\ Body k's length cell at the next cell boundary, its bytes and the byte past.
: LAY-BODY ( n -- )
   {: k:n :}
   LAID-END @ CELL 1- +  CELL negate and {: at:n :}
   at CELL + {: img:n :}
   img k LAID-AT !
   img  k STR-AT @ BODY-U +  1 +  CELL 1- +  CELL negate and  LAID-END ! ;

: LAY-BODIES ( -- )
   STR-N @ LAID-AT-RESERVE
   0
   STR-N @ 0 ?do
      0 i LAID-AT !
      i LAID? if  dup 1 + BY-BODY-RESERVE  i over BY-BODY !  1 +  then
   loop
   {: n:n :}
   n 0 > if 0 BY-BODY n [: BODY< ;] SORT:SORT! then
   n 0 ?do i BY-BODY @ LAY-BODY loop ;

\ A stripped program's image data: the header, the live units, the laid bodies.
: LAY-DATA ( -- )
   OPEN-DP @ BASE @ - LAID-END !
   LAY-UNITS  LAY-BODIES ;


\ The units the program reaches from the header, in rounds of reads until one
\ makes no unit live and reaches no piece.
: REACH-DATA ( -- )
   UNITS  STRINGS
   HEADER-UNIT REACH-UNIT
   begin
      0 WENT-LIVE !
      READ-CELLS  READ-EMS
      WENT-LIVE @ 0=
   until
   ENGINE? @ 0= if LAY-DATA then ;


: DATA-IMAGE-LEN ( -- n )
   ENGINE? @ if SPACE-DATA nip exit then
   LAID-END @ ;

\ The segment holding the byte at v or ending at it, -1 for none.
: SEG-OF ( n -- n )
   {: v:n :}
   0 SEG-N @
   begin 2dup < while
      {: lo:n hi:n :}
      lo hi + 2 / {: mid:n :}
      mid 0 SEG @ v <= if mid 1 + hi else lo mid then
   repeat
   drop 1 - {: s:n :}
   s 0< if -1 exit then
   s 0 SEG @  s 1 SEG @ +  v >= if s else -1 then ;

\ Host address v's offset in a stripped program's image data; past the
\ space's end, the profiler band and ceiling stay where they are.
: LAID-OFFSET ( n -- n )
   {: v:n :}
   v STRING-OF {: k:n :}
   k 0 >= if k LAID-AT @ else 0 then {: img:n :}
   img 0<> if img  v +  k STR-AT @ - exit then
   v SEG-OF {: s:n :}
   s 0 >= if s 2 SEG @  v +  s 0 SEG @ - exit then
   v OPEN-DP @ < if v BASE @ - exit then
   SPACE-END {: end:n :}
   v end = if LAID-END @ exit then
   v end > if v BASE @ - exit then
   s" link: a DATA pointer into data the link dropped: " SAY v SAY-INT SAY-CR
   s" link: a DATA pointer into data the link dropped" 74 die ;

\ A host address in the open space as its offset in the image's data: an
\ engine's from the space's start.
: DATA-OFFSET ( n -- n )
   {: v:n :}
   v BASE @ - {: off:n :}
   off 0<  off SPACE-SIZE @ >=  or if
      s" link: a DATA pointer outside the build's space: " SAY v SAY-INT SAY-CR
      s" link: a DATA pointer outside the build's space" 74 die
   then
   ENGINE? @ if off exit then
   v LAID-OFFSET ;

\ Address row n's cell's offset in the image, and the row with that offset.
: IMAGE-OFF ( n -- n ) ROW-OFF BASE @ + DATA-OFFSET ;
: IMAGE-ROW ( n -- n )
   dup SNAP-RELOC:XTCELL-OFF-MASK invert and  swap IMAGE-OFF or ;


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

: COPY-SPACE ( -- )
   SPACE-DATA {: p:ptr u:n :}
   p 0 COPY-AT u BYTE-COPY ;

\ The space's u bytes from offset from at image offset at.
: COPY-BYTES-AT ( n n n -- )
   {: from:n at:n u:n :}
   from SPACE-BYTES  at COPY-AT  u BYTE-COPY ;

\ The header, each segment and each laid body with its length cell.
: COPY-LAID ( -- )
   0 0  OPEN-DP @ BASE @ -  COPY-BYTES-AT
   SEG-N @ 0 ?do  i 0 SEG @ BASE @ -  i 2 SEG @  i 1 SEG @  COPY-BYTES-AT  loop
   STR-N @ 0 ?do
      i LAID-AT @ {: img:n :}
      img 0<> if
         i STR-AT @ CELL - BASE @ -  img CELL -  CELL i STR-AT @ BODY-U +  COPY-BYTES-AT
      then
   loop ;

: FILL-COPY ( -- )
   COPY-N {: n:n :}
   n COPY-CELLS-RESERVE
   ENGINE? @ if  0 n 1- COPY-CELLS !  COPY-SPACE exit  then
   n 0 ?do 0 i COPY-CELLS ! loop
   COPY-LAID ;


\ The copy's bytes [from, to), as offsets from the space, zeroed.
: ZERO-COPY ( n n -- )
   {: from:n to:n :}
   to from ?do 0 i COPY-AT c! loop ;

\ Each body in the header the program does not keep zeroed, its length cell
\ with it.
: CUT-HEADER-STRINGS ( -- )
   STR-N @ 0 ?do
      i STR-AT @ {: b:n :}
      b CELL - {: at:n :}
      at BASE @ -  {: from:n :}
      from 0 >=  at OPEN-DP @ <  and  i STR-KEPT @ 0=  and if
         from  from CELL + b BODY-U +  ZERO-COPY
      then
   loop ;


\ The rows list's cell index in the copy, as the header's base field names it.
: LIST-AT ( -- n )
   SNAP-RELOC:XTCELL-N-CELL ADDRESS-CELLS:BASE-FIELD +  SPACE-AT @ CELL / ;

\ Whether address row i's cell lies in a live unit, whose bytes the image lays.
: CELL-LAID? ( n -- bool )
   CELL-UNIT @ {: u:n :}
   u 0< if false exit then
   u REACHED? ;

\ A stripped program's copy lists only its kept rows, at their image offsets;
\ a dropped row's laid cell and its list slot hold 0.
: KEEP-LIST ( -- )
   0 KEPT-N !
   ROWS {: rows:ptr count:n :}
   count 0 ?do
      rows i cells + @ {: row:n :}
      i ROW-KEPT? if
         row IMAGE-ROW  LIST-AT KEPT-N @ +  COPY-CELLS !  1 KEPT-N +!
      else
         i CELL-LAID? if 0 row IMAGE-OFF COPY-AT LE:U64! then
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
   ENGINE? @ 0= if CUT-HEADER-STRINGS KEEP-LIST then
   SORT-ROWS ;

;package
