split-str
\ link-copy.f - the data a program reaches and the link's copy of the open
\ space: REACH-DATA marks the units and address rows a stripped program reads
\ and lays its data out; the copy is that data, its kept rows listed in cell
\ order. A machine's writer gives each kept cell its image value.
\
\ THE DATA A STRIPPED PROGRAM REACHES.
\ A stripped program keeps the extents of the build's data it reaches: unit b,
\ b < BUILT-EXTS-N, is BUILT-EXTS row b; unit BUILT-EXTS-N the header data
\ [BASE, OPEN-DP); unit BUILT-EXTS-N + 1 the data no piece holds, the build
\ body's. A piece's extents tile its data but for the cell LAY sets apart
\ before its first laid text: no unit holds it, and a build whose cell holds an
\ address there is refused with LINKER:E-ADDRESS. The header is live but for
\ its string bodies, and a header address cell is read once no piece stored it
\ or a reached one did: a piece is reached once one of its extents or records
\ is live, and an address a piece stored there is that piece's, as its object
\ carries it. The entry cell is read whatever stored it: it is the program's
\ root. A read xt cell roots the code it names and a read pointer cell makes
\ its target's unit live. A placed emission's data address in a live record's
\ range makes its unit live; one only the emission's dead code names lays its
\ unit: a laid unit is laid as a live one is, but its cells are not read and
\ it reaches no piece until a live address makes it live. An address where
\ data ends lies in its extent, since no extent or piece starts where the data
\ before it ends. An address in an interned body or its length cell keeps that
\ body alone, whether live or dead code names it: a header body lies in the
\ header, any other in the piece that first interned it, and every later
\ piece spelling it shares it.
\
\ THE IMAGE'S DATA. A stripped program's is the header as the build left it,
\ its bodies the program does not keep zeroed; then the live and laid units in
\ build order, each laid at the first offset past the one before that keeps its
\ offset modulo a cell, a monotone map, so an interior or end address keeps its
\ meaning; then the kept bodies whose unit is dead, sorted by length then
\ bytes, each from a cell boundary with its length cell and the byte past it.
\ The alignment bytes no unit holds are not laid. An address cell not read
\ holds zeros where it is laid, and the list names none of them. An engine's
\ is the space as is.

require src/core/exit-status.f
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
0 constant DEAD-UNIT
1 constant LAID-UNIT
2 constant LIVE-UNIT
DYNAMIC-BUFFER UNIT-STATE n          \ DEAD-UNIT, LAID-UNIT or LIVE-UNIT
DYNAMIC-BUFFER CELL-UNIT n           \ address row i's cell's unit
DYNAMIC-BUFFER CELL-READ n           \ 1 once address row i is read
DYNAMIC-BUFFER BY-OFF n              \ the address rows in offset order
DYNAMIC-BUFFER EM-LAID n             \ 1 once emission e's data addresses are laid
DYNAMIC-BUFFER REC-READ n            \ 1 once live record r's data addresses are read
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
: UNIT-LIVE? ( n -- bool ) UNIT-STATE @ LIVE-UNIT = ;
\ Whether the image lays unit u: it is live or laid.
: UNIT-KEPT? ( n -- bool ) UNIT-STATE @ DEAD-UNIT <> ;


\ BUILT row r's data span, the data pointer as its piece starts and ends.
: PIECE-SPAN ( n -- n n )
   BUILT-AT BUILT-DP + {: at:^ :}
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
   u UNIT-LIVE? if exit then
   LIVE-UNIT u UNIT-STATE !  1 WENT-LIVE !
   u BUILT-EXTS-N @ < if u 0 BUILT-EXT @ ROW-REACH then ;

: LAY-UNIT ( n -- )
   {: u:n :}
   u 0< if exit then
   u UNIT-KEPT? if exit then
   LAID-UNIT u UNIT-STATE ! ;


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
   0 STR-N @ [: STR-AT @ CELL - v <= ;] SORT:PARTITION-POINT
   1 - {: k:n :}
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
   NULL-PTR BYTE-VIEW a + {: p:^ :}
   NULL-PTR BYTE-VIEW b + {: q:^ :}
   au 0 ?do
      p i + c@  q i + c@  2dup <> if < unloop exit then  2drop
   loop
   false ;


\ Keeps the body holding data address v, true when one does.
: STR-KEEP ( n -- bool )
   STRING-OF {: k:n :}
   k 0< if false exit then
   1 k STR-KEPT !  true ;

: REACH-ADDRESS ( n -- )
   {: v:n :}
   v STR-KEEP if exit then
   v UNIT-OF REACH-UNIT ;

: LAY-ADDRESS ( n -- )
   {: v:n :}
   v STR-KEEP if exit then
   v UNIT-OF LAY-UNIT ;


\ Address row n's cell's offset.
: NTH-OFF ( n -- n )
   {: n:n :}
   ROWS drop n cells + @ ROW-OFF ;

\ Each address row's cell's unit; no unit live, no piece reached.
: UNITS ( -- )
   BUILT-N ROW-REACHED-RESERVE
   BUILT-N 0 ?do 0 i ROW-REACHED ! loop
   BUILT-EXTS-N @ 2 + {: units:n :}
   units UNIT-STATE-RESERVE
   units 0 ?do DEAD-UNIT i UNIT-STATE ! loop
   ROWS {: rows:^ count:n :}
   count CELLS-N !
   count CELL-UNIT-RESERVE  count CELL-READ-RESERVE
   count 0 ?do
      rows i cells + @ ROW-OFF BASE @ + UNIT-OF  i CELL-UNIT !
      0 i CELL-READ !
   loop
   count BY-OFF-RESERVE
   count 0 ?do i i BY-OFF ! loop
   count 0 > if 0 BY-OFF count [: swap NTH-OFF swap NTH-OFF < ;] SORT:SORT! then
   NSHADOW:EMISSIONS EM-LAID-RESERVE
   NSHADOW:EMISSIONS 0 ?do 0 i EM-LAID ! loop
   NSHADOW:RECORDS REC-READ-RESERVE
   NSHADOW:RECORDS 0 ?do 0 i REC-READ ! loop ;


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
   u UNIT-LIVE? 0= if false exit then
   u HEADER-UNIT <> if true exit then
   row ROW-OFF {: off:n :}
   off APP-ENTRY:XT-CELL = if true exit then
   off STORED? 0= if true exit then
   off STORED-LIVE? ;

\ Rows are read in offset order: a cold build lists them in mark order and an
\ object in extent order.
: READ-CELLS ( -- )
   ROWS {: rows:^ count:n :}
   count CELLS-N @ <> if E-COMP-STATE throw then
   count 0 ?do
      i BY-OFF @ {: k:n :}
      rows k cells + @ {: row:n :}
      k CELL-READ @ 0= if
         k row ROW-LIVE? if  1 k CELL-READ !  row READ-ROW  then
      then
   loop ;


\ Each data address emission e holds at an offset in [from, to), handed to visit.
: EACH-DATA ( n n n [ n -- ] -- )
   {: e:n from:n to:n visit :}
   e NSHADOW:ADDR-SITES 0 ?do
      e i NSHADOW:ADDR-SITE-KIND@ NEMIT:ADDR-DATA = if
         e i NSHADOW:ADDR-SITE@ {: at:n :}
         at from >=  at to < and if
            e i SITE-VALUE {: v:n has:bool :}
            has if v visit execute then
         then
      then
   loop ;

\ Every site of a placed emission resolves, so each one lays its unit.
: LAY-EM ( n -- )
   {: e:n :}
   1 e EM-LAID !
   e 0 e NSHADOW:SIZE ['] LAY-ADDRESS EACH-DATA ;

\ Live record r's data addresses, in its range, and its piece reached.
: READ-REC ( n -- )
   {: r:n :}
   1 r REC-READ !
   r NSHADOW:EMISSION@  r NSHADOW:ENTRY@  r RANGE-END
   ['] REACH-ADDRESS EACH-DATA
   r NSHADOW:RECORD@ REC-ROW@ if ROW-REACH else drop then ;

: READ-EMS ( -- )
   NSHADOW:EMISSIONS 0 ?do
      i PLACED @ 0<>  i EM-LAID @ 0= and if i LAY-EM then
   loop
   NSHADOW:RECORDS 0 ?do
      i LIVE @ 0<>  i REC-READ @ 0= and if i READ-REC then
   loop ;


\ The build's DP, where the space's data ends.
: SPACE-END ( -- n ) SPACE-DATA nip BASE @ + ;

: SEG ( n n -- ^n ) swap SEG-CELLS * + SEGS ;

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

\ BUILT-EXTS row b, laid when kept.
: LAY-EXT ( n -- )
   {: b:n :}
   b UNIT-KEPT? if b EXT-SPAN SEG+ then ;

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

\ The kept units from OPEN-DP in data order: the extents, and when the rest
\ unit is kept each span no piece holds, before, between and after the pieces.
: LAY-UNITS ( -- )
   SORT-EXTS  SORT-PIECES
   0 SEG-N !  0 EXT-I !
   REST-UNIT UNIT-KEPT? {: rest:bool :}
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
   u UNIT-KEPT? 0= ;

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

\ A stripped program's image data: the header, the kept units, the laid bodies.
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
   0 SEG-N @ [: 0 SEG @ v <= ;] SORT:PARTITION-POINT
   1 - {: s:n :}
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
   s" E-LINK-DATA-POINTER" REJECTION:OPEN
   s" the data address " REJECTION:EMIT-STR  v REJECTION:EMIT-N
   s"  lies in no data the program keeps" REJECTION:EMIT-STR
   REJECTION:HELP
   s" point into the data a variable, buffer or allot made, never at the bytes between"
   REJECTION:EMIT-STR
   EXIT-STATUS:REJECTED REJECTION:STOP ;

\ A host address in the open space as its offset in the image's data: an
\ engine's from the space's start.
: DATA-OFFSET ( n -- n )
   {: v:n :}
   v BASE @ - {: off:n :}
   off 0<  off SPACE-SIZE @ >=  or if
      s" E-LINK-DATA-POINTER" REJECTION:OPEN
      s" the data address " REJECTION:EMIT-STR  v REJECTION:EMIT-N
      s"  lies outside the build's space" REJECTION:EMIT-STR
      REJECTION:HELP
      s" store in a DATA pointer cell only 0 or an address in the data the build allots"
      REJECTION:EMIT-STR
      EXIT-STATUS:REJECTED REJECTION:STOP
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
   DATA-IMAGE-LEN {: len:n :}
   off CELL + len > if
      s" E-LINK-DATA-CELL" REJECTION:OPEN
      s" the declared cell at offset " REJECTION:EMIT-STR  off REJECTION:EMIT-N
      s"  lies past the program's " REJECTION:EMIT-STR  len REJECTION:EMIT-N
      s"  bytes of data" REJECTION:EMIT-STR
      REJECTION:HELP
      s" declare a cell (`xt!`, `ptr-cell-mark`) only in data the build allotted, below `here`"
      REJECTION:EMIT-STR
      EXIT-STATUS:REJECTED REJECTION:STOP
   then ;

: EACH-KEPT ( [ n -- ] -- )
   {: visit :}
   ROWS {: rows:^ count:n :}
   count 0 ?do i ROW-KEPT? if rows i cells + @ visit execute then loop ;


: COPY-N ( -- n ) DATA-IMAGE-LEN CELL 1- + CELL / ;
: COPY-AT ( n -- ^u8 ) 0 COPY-CELLS BYTE-VIEW swap + ;

: COPY-SPACE ( -- )
   SPACE-DATA {: p:^ u:n :}
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

\ Whether address row i's cell lies in a kept unit, whose bytes the image lays.
: CELL-LAID? ( n -- bool )
   CELL-UNIT @ {: u:n :}
   u 0< if false exit then
   u UNIT-KEPT? ;

\ A stripped program's copy lists only its kept rows, at their image offsets;
\ a dropped row's laid cell and its list slot hold 0.
: KEEP-LIST ( -- )
   0 KEPT-N !
   ROWS {: rows:^ count:n :}
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
