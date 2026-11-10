\ link-data.f - a build's DATA: a copy of the open space's cells, each
\ declared cell given its value at DATA-VA, appended to A64ICODE's code buffer
\ after the routine band as its non-zero runs, then the DATA list, the offset
\ from DATA-VA of every cell holding a code address, which start-up adds the
\ load offset to. The link also writes the header cells HEADER-CELLS names and
\ lists the code ones. An engine's link adds the records: the product kernel's
\ primitives, then the build's, each build record and wordlist id renumbered
\ for the product (RECORD-IMAGE), and the record list.
\ LINKER:WRITE writes the linked program; the other words are LINKER's own.

require src/lib/error.f
require src/lib/le.f
require src/lib/sort.f
require src/lib/fs-mutate.f
require src/habu/layout.f
require src/habu/boot-cells.f
require src/habu/wl.f
require src/habu/address-cells.f
require src/arch/arm64/icode.f
require src/arch/arm64/link-kernel.f
require src/habu/fdio.f
require src/habu/sign-id.f
HB-TARGET-LINUX? [if]
   require src/os/linux/elf.f
   require src/os/linux/sign.f
[then]
HB-TARGET-MACOS? [if]
   require src/os/macos/macho.f
   require src/os/macos/sign2.f
[then]
require src/habu/driver-io.f
require src/habu/linker.f
require src/habu/link-band.f
require src/habu/link-map.f
using DRIVER

package LINKER
private

\ The kernel object's labels (src/arch/arm64/kernel-object.f), which ADOPT-LABELS
\ sets. The tail's, which the start-up reads: the DATA image's start and end,
\ the DATA list's start and end.
variable LIMAGE
variable LIMAGE-END
variable LLIST
variable LLIST-END
\ An engine's: the record image's start and end, the record list's start and end.
variable LRECS
variable LRECS-END
variable LRLIST
variable LRLIST-END
\ The kernel's uncaught-throw reporter, signal handler, primitive count and
\ dictionary table; an engine's throw recoveries.
variable LUNCAUGHT
variable LSIGH
variable LNCOUNT
variable LDICT
variable LEVALREC
variable LRREC
variable LFLOORREC
\ The kernel's code end, in bytes from the text's start: the product's words follow.
variable KERNEL-END

\ The record list's offsets from the dictionary band, ascending as the records
\ are laid; the code offset of each build record's out-of-line name.
DYNAMIC-BUFFER RLISTED n
variable RLISTED-N
DYNAMIC-BUFFER NAME-AT n
\ Each kernel primitive's facts text, as its offset from the space, 0 for none.
DYNAMIC-BUFFER PRIM-FACTS-AT n


\ A bound label's byte offset in A64ICODE's code buffer.
: LABEL-OFFSET ( ptr n -- n ) A64ICODE:LABEL@ ARM64:OFFSET ;


\ The image is the space's runs: its length is the space's, not the distance
\ between its labels.
: DATA-IMAGE-AT ( -- n ) LIMAGE LABEL-OFFSET ;
: DATA-IMAGE-LEN ( -- n ) SPACE-DATA nip ;
: DATA-LIST-AT ( -- n ) LLIST LABEL-OFFSET ;
: DATA-LIST-N ( -- n ) LLIST-END LABEL-OFFSET DATA-LIST-AT - 8 / ;


\ A host address in the open space at its DATA-VA address: the resolver code's
\ data address sites get.
: DATA-ADDRESS ( n -- n )
   {: v:n :}
   v BASE @ - {: off:n :}
   off 0 >=  off SPACE-SIZE @ < and if DATA-VA VA>N off + exit then
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
\ (src/habu/intern-table.f). DATA-IMAGE zeroes the dead units, all but their
\ kept bodies, and every address cell not read, and neither copies nor lists
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


: PAD8 ( -- ) A64ICODE:ASM-LEN 7 and 0<> if 0 A64ICODE:EMITW then ;

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


\ The link address xt cell off's value v gives the image.
: XT-VALUE ( n n -- n )
   {: off:n v:n :}
   v 0= if 0 exit then
   v LINK-ADDRESS {: to:n :}
   to -1 <> if to exit then
   s" link: the xt cell at offset " SAY off SAY-INT s"  holds " SAY v SAY-INT SAY-CR
   s" link: an xt cell names a routine the product does not place" 74 die ;


\ The image copy of row's cell: an xt cell's link address, a DATA pointer's
\ DATA-VA address, a scratch cell's 0.
: COPY-ROW ( n -- )
   {: row:n :}
   row ROW-OFF {: off:n :}
   off CELL + DATA-IMAGE-LEN > if
      s" link: a declared cell past the build's data, at offset " SAY off SAY-INT SAY-CR
      s" link: a declared cell lies past the build's data" 74 die
   then
   row CELL-VALUE {: v:n :}
   row SCRATCH-ROW? if 0 off COPY-AT LE:U64! exit then
   row DATA-ROW? if
      v 0<> if v DATA-ADDRESS off COPY-AT LE:U64! then exit
   then
   off v XT-VALUE off COPY-AT LE:U64! ;


\ A cell holding 0 holds no code address and is not listed. LIST, sorts the list
\ and SORT-ROWS the copied rows by cell: neither follows the build's mark order.
: LIST-ROW ( n -- )
   {: row:n :}
   row XT-ROW? 0= if exit then
   row CELL-VALUE 0= if exit then
   LISTED-N @ 1 + LISTED-RESERVE
   row ROW-OFF LISTED-N @ LISTED !  1 LISTED-N +! ;

: LIST, ( -- )
   LISTED-N @ {: n:n :}
   n 0 > if 0 LISTED n [: < ;] SORT:SORT! then
   n 0 ?do i LISTED @ A64ICODE:DCQ, loop ;


\ A cell the build stored into would be written and slid twice.
: HEADER! ( n n -- )
   {: v:n off:n :}
   off SPACE-AT @ 0<> if
      s" link: the build stored into the header cell at offset " SAY off SAY-INT SAY-CR
      s" link: a build stored into a header cell the link writes" 74 die
   then
   v off COPY-AT LE:U64! ;


\ Header cells valued as the product's boot leaves them (habu2.f EM-DATA-INIT,
\ EM-STARTUP-RUNTIME-STATE), at the kernel object's offsets, and the cells it
\ presets; CODE-END-CELL ends the kernel, before the product's words.
: HEADER-CELLS ( -- )
   DATA-VA VA>N {: va:n :}
   s" DATA-START" ARM64:SYM {: start:n :}
   va DATA-IMAGE-LEN +  s" DP-CELL" ARM64:SYM HEADER!
   va start +  s" DP-FLOOR-CELL" ARM64:SYM HEADER!
   va s" DP-CEILING-OFF" ARM64:SYM +  s" DP-CEILING-CELL" ARM64:SYM HEADER!
   start s" HEAP-START-CELL" ARM64:SYM HEADER!
   ARM64:PRESET-N 0 ?do i ARM64:PRESET@ HEADER! loop
   va s" FD-CELL" ARM64:SYM +  s" FD-PTR-CELL" ARM64:SYM HEADER!
   TEXT-VA s" RBASE-CELL" ARM64:SYM HEADER!
   TEXT-VA LUNCAUGHT LABEL-OFFSET +  s" UNCGH-CELL" ARM64:SYM HEADER!
   TEXT-VA LSIGH LABEL-OFFSET +  s" STUB-CELL" ARM64:SYM HEADER!
   TEXT-VA KERNEL-END @ +  s" CODE-END-CELL" ARM64:SYM HEADER!
   \ The build's MARK left its own index mapping's address here; the product's
   \ first MARK builds one from 0.
   0 ADDRESS-CELLS:INDEX-CELL COPY-AT LE:U64! ;


\ An engine's throw recoveries, as hb's boot stores them (habu2.f
\ EM-STARTUP-RUNTIME-STATE): its kernel's throw branches through these cells
\ inside an evaluate, at a live tty REPL and after a data-stack underdepth. A
\ stripped program's cut keeps none of the three.
: RECOVERY-CELLS ( -- )
   TEXT-VA LEVALREC LABEL-OFFSET +  s" EVALREC-CELL" ARM64:SYM HEADER!
   TEXT-VA LRREC LABEL-OFFSET +  s" RRECP-CELL" ARM64:SYM HEADER!
   TEXT-VA LFLOORREC LABEL-OFFSET +  s" FLOORREC-CELL" ARM64:SYM HEADER! ;

\ The header cells HEADER-CELLS and RECOVERY-CELLS give a code address.
: LIST-HEADER ( -- )
   s" RBASE-CELL" ARM64:SYM A64ICODE:DCQ,
   s" UNCGH-CELL" ARM64:SYM A64ICODE:DCQ,
   s" STUB-CELL" ARM64:SYM A64ICODE:DCQ,
   s" CODE-END-CELL" ARM64:SYM A64ICODE:DCQ,
   ENGINE? @ if
      s" EVALREC-CELL" ARM64:SYM A64ICODE:DCQ,
      s" RRECP-CELL" ARM64:SYM A64ICODE:DCQ,
      s" FLOORREC-CELL" ARM64:SYM A64ICODE:DCQ,
   then ;


\ ---- an engine's records ------------------------------------------------------------
\ A record index holds only in the process that made it: the product numbers its
\ kernel's primitives [0, P) as EMIT-DICT lays them and each build record r at
\ P + (r - FIRST-REC). link-map.f WID-MAP numbers the product's wordlist ids.

\ The product's WIDN-CELL.
: PRODUCT-WIDN ( -- n ) FIRST-DYNAMIC-WID  WIDN-CELL HOST@ +  FIRST-WID - ;


\ The product kernel's primitive count and record k's cell, as EMIT-DICT laid
\ them: [0] and [8] the entry's and the last word's code offsets, [24] an
\ out-of-line name's.
: PRIMS ( -- n ) A64ICODE:CODE LNCOUNT LABEL-OFFSET + LE:U64@ ;

: PRIM@ ( n n -- n )
   {: k:n slot:n :}
   A64ICODE:CODE LDICT LABEL-OFFSET +  k DREC * +  slot cells +  LE:U64@ ;

\ Primitive k's name: at code offset [24] when out of line, else in [24].
: PRIM-NAME$ ( n -- ptr u8 n )
   {: k:n :}
   k 2 PRIM@ {: flags:n :}
   flags DNAME-EXT and 0<> if
      A64ICODE:CODE k 3 PRIM@ +
   else
      A64ICODE:CODE LDICT LABEL-OFFSET +  k DREC * +  24 +
   then
   flags DNAME-LEN-MASK and ;


\ A built-in primitive's facts text, laid in the space; 0 for a package row, an
\ owned primitive or a primitive no text says.
: PRIM-TEXT ( n -- n )
   {: k:n :}
   k 5 PRIM@ WL:PRIMITIVES <> if 0 exit then
   k PRIM-NAME$ CHECK:PRIMITIVE-FACTS {: a:ptr u:n :}
   u 0= if 0 exit then
   a u INTERN:TEXT BASE @ - ;

\ After the kernel and before DATA-IMAGE copies the space.
: PRIM-TEXTS ( -- )
   PRIMS PRIM-FACTS-AT-RESERVE
   PRIMS 0 ?do i PRIM-TEXT i PRIM-FACTS-AT ! loop ;

: RLIST+ ( n -- )
   RLISTED-N @ 1 + RLISTED-RESERVE
   RLISTED-N @ RLISTED !  1 RLISTED-N +! ;

\ A cell holding a code address at band offset off: listed, so start-up slides it.
: CODE-CELL, ( n n -- )
   {: v:n off:n :}
   v A64ICODE:DCQ,  off RLIST+ ;


\ Primitive k's body length, as PRIM-REC, writes it.
: PRIM-LEN ( n -- n ) dup 1 PRIM@ swap 0 PRIM@ - 4 - ;

\ As habu2.f EM-SEED-DICT seeds it: [8] the body's length, a package row's
\ [0] and [8] its wordlist ids; [32] the facts text PRIM-TEXTS laid.
: PRIM-REC, ( n -- )
   {: k:n :}
   k DREC * {: at:n :}
   k 0 PRIM@ {: first:n :}
   k 5 PRIM@ {: wid:n :}
   wid WL:NAMESPACE = if
      first A64ICODE:DCQ,  k 1 PRIM@ A64ICODE:DCQ,
   else
      TEXT-VA first + at CODE-CELL,
      k PRIM-LEN A64ICODE:DCQ,
   then
   k 2 PRIM@ {: flags:n :}
   flags A64ICODE:DCQ,
   flags DNAME-EXT and 0<> if
      TEXT-VA k 3 PRIM@ +  at 24 + CODE-CELL,
   else
      k 3 PRIM@ A64ICODE:DCQ,
   then
   k PRIM-FACTS-AT @ A64ICODE:DCQ,
   wid A64ICODE:DCQ, ;


\ Build record rec's length: the product kernel's for a record entering a
\ kernel primitive, else its own.
: BUILD-LEN ( ptr n -- n )
   {: rec:ptr :}
   rec XREF-START ARM64:KERNEL-ROW {: k:n :}
   k 0 >= if k PRIM-LEN exit then
   rec XREF-RAW-LEN ;


\ Build record r's entry at the product's link address; 0 stays 0.
: ENTRY-ADDRESS ( n -- n )
   {: r:n :}
   r XREF-REC XREF-START {: a:n :}
   a 0= if 0 exit then
   a LINK-ADDRESS {: to:n :}
   to -1 <> if to exit then
   s" link: record " SAY r XREF-REC XREF-NAME$ SAY s"  enters at " SAY a SAY-INT
   s" , which the product does not place" SAY SAY-CR
   s" link: a record enters code the product does not place" 74 die ;


\ Build record r at product index k: [32], the facts text's offset from the
\ image's DATA, holds in the product's.
: BUILD-REC, ( n n -- )
   {: r:n k:n :}
   r XREF-REC {: rec:ptr :}
   k DREC * {: at:n :}
   rec XREF-WORDLIST {: wid:n :}
   wid XREF:PACKAGE-WL? if
      rec XREF-PKG-PUBLIC r WID-MAP A64ICODE:DCQ,
      rec XREF-PKG-PRIVATE r WID-MAP A64ICODE:DCQ,
   else
      r ENTRY-ADDRESS {: entry:n :}
      entry 0<> if entry at CODE-CELL, else 0 A64ICODE:DCQ, then
      rec BUILD-LEN A64ICODE:DCQ,
   then
   rec XREF-FLAGS A64ICODE:DCQ,
   rec XREF-EXT? if
      TEXT-VA  r FIRST-REC @ - NAME-AT @ +  at 24 + CODE-CELL,
   else
      rec XREF-NAME-SLOT XREF-CELL@ A64ICODE:DCQ,
   then
   rec CK-REC-FACTS-SLOT XREF-CELL@ A64ICODE:DCQ,
   wid r WID-MAP A64ICODE:DCQ, ;


\ Each build record's out-of-line name, once, before the records.
: NAME-POOL ( -- )
   ndict@ FIRST-REC @ - NAME-AT-RESERVE
   ndict@ FIRST-REC @ ?do
      i XREF-REC {: rec:ptr :}
      rec XREF-EXT? if
         A64ICODE:ASM-LEN  i FIRST-REC @ - NAME-AT !
         rec XREF-NAME$ A64ICODE:BYTES,
      then
   loop ;


\ The product's record count.
: PRODUCT-NDICT ( -- n ) PRIMS  ndict@ +  FIRST-REC @ - ;


\ After DATA-IMAGE: the names, then at an 8-byte boundary the records, then
\ the record list.
: RECORD-IMAGE ( -- )
   NAME-POOL
   PAD8
   0 RLISTED-N !
   LRECS A64ICODE:LABEL@ A64ICODE:LBL,
   PRIMS 0 ?do i PRIM-REC, loop
   ndict@ FIRST-REC @ ?do  i  PRIMS i + FIRST-REC @ -  BUILD-REC,  loop
   LRECS-END A64ICODE:LABEL@ A64ICODE:LBL,
   LRLIST A64ICODE:LABEL@ A64ICODE:LBL,
   RLISTED-N @ 0 ?do i RLISTED @ A64ICODE:DCQ, loop
   LRLIST-END A64ICODE:LABEL@ A64ICODE:LBL,
   RLISTED-RELEASE  NAME-AT-RELEASE  PRIM-FACTS-AT-RELEASE ;


\ An engine's header cells beside HEADER-CELLS': the record count the seal
\ covers, the wordlist counter and the types wordlist, as the product numbers them.
: ENGINE-CELLS ( -- )
   PRODUCT-NDICT s" SEAL-NDICT-CELL" ARM64:SYM HEADER!
   PRODUCT-WIDN s" WIDN-CELL" ARM64:SYM HEADER!
   \ as WID-MAP numbers a minted id
   FIRST-DYNAMIC-WID BOOT:TYPES-WL FIRST-WID - +  s" TYPES-WL-CELL" ARM64:SYM HEADER! ;


: SET? ( n -- bool ) COPY-CELLS @ 0<> ;

\ The first non-zero cell at or after cell i, or COPY-N.
: NEXT-SET ( n -- n )
   begin dup COPY-N < if dup SET? 0= else false then while 1+ repeat ;

\ The cell after the run whose first cell is i. A run's header costs two
\ cells, so a gap of one or two zero cells stays inside it and three end it.
: RUN-END ( n -- n )
   begin dup 1+ NEXT-SET  2dup swap - 3 <=  over COPY-N < and while nip repeat
   drop 1+ ;

: RUN, ( n n -- )
   {: first:n end:n :}
   first CELL * A64ICODE:DCQ,  end first - A64ICODE:DCQ,
   end first ?do i COPY-CELLS @ A64ICODE:DCQ, loop ;

\ The copy as runs [off u64][n u64][n cells], off the run's byte offset from
\ DATA; zero cells outside a run are not written (boot.f COPY-DATA,).
: RUNS, ( -- )
   0 NEXT-SET
   begin dup COPY-N < while dup RUN-END tuck RUN, NEXT-SET repeat drop ;


\ After PLACE-ROUTINES: at an 8-byte boundary the copy's runs with the header
\ cells written, then the DATA list, the code header cells last.
: DATA-IMAGE ( -- )
   PAD8
   LIMAGE A64ICODE:LABEL@ A64ICODE:LBL,
   FILL-COPY
   ENGINE? @ 0= if CUT-DEAD KEEP-STRINGS KEEP-LIST then
   SORT-ROWS
   ['] COPY-ROW EACH-KEPT
   HEADER-CELLS
   ENGINE? @ if ENGINE-CELLS RECOVERY-CELLS then
   RUNS,
   LIMAGE-END A64ICODE:LABEL@ A64ICODE:LBL,
   LLIST A64ICODE:LABEL@ A64ICODE:LBL,
   0 LISTED-N !  ['] LIST-ROW EACH-KEPT
   LIST,
   LIST-HEADER
   LLIST-END A64ICODE:LABEL@ A64ICODE:LBL, ;


\ The kernel object's start-up and kernel, adopted, and the labels the link reads.
: ADOPT-LABELS ( -- )
   ARM64:ADOPT
   s" IMAGE" ARM64:SYM-LABEL LIMAGE A64ICODE:LABEL!
   s" IMAGE-END" ARM64:SYM-LABEL LIMAGE-END A64ICODE:LABEL!
   s" LIST" ARM64:SYM-LABEL LLIST A64ICODE:LABEL!
   s" LIST-END" ARM64:SYM-LABEL LLIST-END A64ICODE:LABEL!
   s" UNCAUGHT" ARM64:SYM-LABEL LUNCAUGHT A64ICODE:LABEL!
   s" SIGH" ARM64:SYM-LABEL LSIGH A64ICODE:LABEL!
   s" NCOUNT" ARM64:SYM-LABEL LNCOUNT A64ICODE:LABEL!
   s" DICT" ARM64:SYM-LABEL LDICT A64ICODE:LABEL!
   ENGINE? @ 0= if exit then
   s" RECS" ARM64:SYM-LABEL LRECS A64ICODE:LABEL!
   s" RECS-END" ARM64:SYM-LABEL LRECS-END A64ICODE:LABEL!
   s" RLIST" ARM64:SYM-LABEL LRLIST A64ICODE:LABEL!
   s" RLIST-END" ARM64:SYM-LABEL LRLIST-END A64ICODE:LABEL!
   s" EVALREC" ARM64:SYM-LABEL LEVALREC A64ICODE:LABEL!
   s" RREC" ARM64:SYM-LABEL LRREC A64ICODE:LABEL!
   s" FLOORREC" ARM64:SYM-LABEL LFLOORREC A64ICODE:LABEL! ;


\ A stripped program's code: the start-up, the kernel code the program reaches,
\ the routines and the data it reaches. HEADER-CELLS reads LUNCAUGHT and LSIGH
\ after the cut.
: LINK-STRIPPED ( -- )
   ADOPT-LABELS
   ROUTINES
   REACH-DATA
   LUNCAUGHT A64ICODE:LABEL@ ARM64:ROOT-LABEL
   LSIGH A64ICODE:LABEL@ ARM64:ROOT-LABEL
   ARM64:CUT
   A64ICODE:ASM-CP @ 4 * KERNEL-END !
   ['] DATA-ADDRESS PLACE-ROUTINES
   DATA-IMAGE ;


\ C2-MEM's word whose entry scope-kind? answers kind k for.
: SCOPE-WORD$ ( n -- ptr u8 n )
   {: k:n :}
   k 2 = if s" WITH-READ" exit then
   k 3 = if s" WITH-MUT" exit then
   k 4 = if s" WITH-MUT-LOAN" exit then
   k 5 = if s" WITH-INIT" exit then
   k 6 = if s" WITH-RECORDS" exit then
   k 7 = if s" WITH-RECORD" exit then
   k 8 = if s" WITH-FIELD" exit then
   s" link: scope-kind? has no kind past 8" 74 die ;


\ Kind k's chain in the kernel becomes adrp+add of the entry of k's word in
\ C2-MEM's public wordlist wid.
: SCOPE-ENTRY! ( n n -- )
   {: wid:n k:n :}
   k SCOPE-WORD$ wid CHECKER-RESOLVE:WL-PROBE {: rec:ptr :}
   rec XREF-FOUND? 0= if
      s" link: C2-MEM has no " SAY k SCOPE-WORD$ SAY SAY-CR
      s" link: C2-MEM lacks a scope entry" 74 die
   then
   rec XREF-START LINK-ADDRESS {: to:n :}
   to -1 = if s" link: a C2-MEM scope entry is code the product does not place" 74 die then
   k ARM64:SCOPE-SITE ARM64:OFFSET {: at:n :}
   to  TEXT-VA at +  A64ICODE:CODE at +  ARM64:CODE-ADDRESS! ;


\ After PLACE-ROUTINES, scope-kind?'s chains (src/arch/arm64/kernel-object.f
\ EMIT) name C2-MEM's entries; a product without C2-MEM leaves them 0.
: SCOPE-ENTRIES ( -- )
   s" C2-MEM" BOOT:NAMESPACE-WL CHECKER-RESOLVE:WL-PROBE {: pkg:ptr :}
   pkg XREF-FOUND? 0= if exit then
   pkg XREF-PKG-PUBLIC {: wid:n :}
   9 2 ?do wid i SCOPE-ENTRY! loop ;


\ An engine's code: the start-up, the whole kernel, every routine, DATA, then
\ the records.
: LINK-ENGINE ( -- )
   ADOPT-LABELS
   A64ICODE:ASM-CP @ 4 * KERNEL-END !
   PRIM-TEXTS
   ROUTINES REACH-ALL
   ['] DATA-ADDRESS PLACE-ROUTINES
   SCOPE-ENTRIES
   DATA-IMAGE
   RECORD-IMAGE ;


\ The kernel object READ read is the kind of kernel this build links.
: OBJECT-KIND ( -- )
   KERNEL? @ if s" link: a kernel build writes no program" 74 die then
   ARM64:READ? 0= if s" link: no kernel object was read for this build" 74 die then
   ENGINE? @ if 1 else 0 then  s" ENGINE" ARM64:SYM <> if
      s" link: the kernel object is another kind of build's" 74 die
   then ;

public

\ Link the open space's program and write it, signed, to the path; the caller
\ first stores its entry, a stripped program's with APP-ENTRY:XT!,
\ an engine's in ENGINE-MAIN:XT-CELL, and writes its names beside it as
\ <path>.map. The facts texts the build's body held lie after every file, so
\ no file's literal can name one. The program goes to a sibling renamed over
\ the path: macOS kills a signed program rewritten in place (docs/bootstrap.md).
: WRITE ( ptr u8 n -- )
   {: path:ptr pathu:n :}
   false GOING
   OBJECT-KIND
   0 CHECK:PLACE-FACTS
   s" ENTRY-CELL" ARM64:SYM {: entry:n :}
   entry SPACE-AT @ 0= if
      s" link: the build stored no entry for the program" 74 die
   then
   ENGINE? @ if LINK-ENGINE else LINK-STRIPPED then
   path pathu [: 2dup ENGINE? @ if SIGN-ID:ENGINE$ else SIGN-ID:PROG$ then 2swap DRV-EMIT-IMAGE
      FS:CHMOD-X ;] FS:REPLACE-STAGED
   path pathu DATA-IMAGE-AT WRITE-MAP ;

;package
;using
