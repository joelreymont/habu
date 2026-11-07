\ link-data.f - a stripped build's DATA: a copy of the open space's cells, each
\ declared cell given its value at DATA-VA, appended to A64ICODE's code buffer
\ after the routine band as its non-zero runs, then the DATA list, the offset
\ from DATA-VA of every cell holding a code address, which start-up adds the
\ load offset to. The link also writes the header cells HEADER-CELLS names and
\ lists the code ones.
\ LINKER:WRITE writes the linked program; the other words are LINKER's own.

require src/lib/error.f
require src/lib/le.f
require src/lib/sort.f
require src/habu/layout.f
require src/habu/boot-cells.f
require src/habu/wl.f
require src/habu/address-cells.f
require src/arch/arm64/icode.f
require src/arch/arm64/link-kernel.f
require src/arch/arm64/boot.f
require src/habu/fdio.f
require src/habu/sign-id.f
require src/habu/driver-io.f
require src/habu/crash.f
require src/habu/habu2.f
require src/habu/linker.f
require src/habu/link-band.f

package LINKER
private

\ The tail's labels, which A64BOOT:START, reads: the DATA image's start and
\ end, the DATA list's start and end.
variable LIMAGE
variable LIMAGE-END
variable LLIST
variable LLIST-END


\ A bound label's byte offset in A64ICODE's code buffer.
: LABEL-OFFSET ( ptr n -- n ) A64ICODE:LABEL@ ARM64:OFFSET ;


\ The image is the space's runs: its length is the space's, not the distance
\ between its labels.
: DATA-IMAGE-AT ( -- n ) LIMAGE LABEL-OFFSET ;
: DATA-IMAGE-LEN ( -- n ) SPACE-DATA nip ;
: DATA-LIST-AT ( -- n ) LLIST LABEL-OFFSET ;
: DATA-LIST-N ( -- n ) LLIST-END LABEL-OFFSET DATA-LIST-AT - 8 / ;


\ A host address in the open space at its DATA-VA address: the resolver code's
\ data address sites get. Native `.` ends its line.
: DATA-ADDRESS ( n -- n )
   {: v:n :}
   v BASE @ - {: off:n :}
   off 0 >=  off DATA-SIZE < and if DATA-VA VA>N off + exit then
   s" link: a DATA pointer outside the build's space: " type v .
   s" link: a DATA pointer outside the build's space" 74 die ;


\ Each row of the space's address cells, handed to visit.
: EACH-ROW ( [ n -- ] -- )
   {: visit :}
   ROWS {: rows:ptr count:n :}
   count 0 ?do rows i cells + @ visit execute loop ;


: ROOT-ROW ( n -- )
   {: row:n :}
   row XT-ROW? 0= if exit then
   row CELL-VALUE {: v:n :}
   v 0<> if v ROOT then ;

\ Every xt cell's value roots the closure.
: ROOT-CELLS ( -- ) ['] ROOT-ROW EACH-ROW ;


: PAD8 ( -- ) A64ICODE:ASM-LEN 7 and 0<> if 0 A64ICODE:EMITW then ;

: COPY-N ( -- n ) DATA-IMAGE-LEN CELL 1- + CELL / ;
: COPY-AT ( n -- ptr u8 ) 0 COPY-CELLS BYTE-VIEW swap + ;

: FILL-COPY ( -- )
   COPY-N {: n:n :}
   n COPY-CELLS-RESERVE
   0  n 1- COPY-CELLS !
   SPACE-DATA {: p:ptr u:n :}
   p 0 COPY-AT u BYTE-COPY ;


\ The copy's rows in cell order, not the build's mark order: the product's first
\ MARK rebuilds the index HEADER-CELLS zeroes.
: SORT-ROWS ( -- )
   SNAP-RELOC:XTCELL-N-CELL ADDRESS-CELLS:BASE-FIELD +  SPACE-AT @ CELL /
   {: at:n :}
   ROWS nip {: n:n :}
   n 0 > if at COPY-CELLS n [: ROW-OFF swap ROW-OFF swap < ;] SORT:SORT! then ;


\ The link address xt cell off's value v gives the image.
: XT-VALUE ( n n -- n )
   {: off:n v:n :}
   v 0= if 0 exit then
   v LINK-ADDRESS {: to:n :}
   to -1 <> if to exit then
   s" link: the xt cell at offset " type off .
   s" link: holds " type v .
   s" link: an xt cell names a routine the product does not place" 74 die ;


\ The image copy of row's cell: an xt cell's link address, a DATA pointer's
\ DATA-VA address, a scratch cell's 0.
: COPY-ROW ( n -- )
   {: row:n :}
   row ROW-OFF {: off:n :}
   off CELL + DATA-IMAGE-LEN > if
      s" link: a declared cell past the build's data, at offset " type off .
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
      s" link: the build stored into the header cell at offset " type off .
      s" link: a build stored into a header cell the link writes" 74 die
   then
   v off COPY-AT LE:U64! ;


\ Header cells valued as hb's boot writes them (habu2.f EM-DATA-INIT,
\ EM-STARTUP-RUNTIME-STATE), but CODE-END-CELL ends the product's own code.
: HEADER-CELLS ( -- )
   DATA-VA VA>N {: va:n :}
   va DATA-IMAGE-LEN +  DP-CELL HEADER!
   va DATA-START +  BOOT:DP-FLOOR-CELL HEADER!
   va DATA-SIZE + PROF-CNT-BYTES -  BOOT:DP-CEILING-CELL HEADER!
   DATA-START BOOT:HEAP-START-CELL HEADER!
   0 BOOT:GLOBAL-WL-CELL HEADER!
   0 BOOT:IMAGE-BASE-CELL HEADER!
   WL:NAMESPACE BOOT:NAMESPACE-WL-CELL HEADER!
   va SIGNAL-ABI:FD-CELL +  SIGNAL-ABI:FD-PTR-CELL HEADER!
   TEXT-VA RBASE-CELL HEADER!
   TEXT-VA LUNCAUGHT LABEL-OFFSET +  UNCGH-CELL HEADER!
   TEXT-VA LSIGH LABEL-OFFSET +  SIGNAL-ABI:STUB-CELL HEADER!
   TEXT-VA DATA-IMAGE-AT +  CODE-END-CELL HEADER!
   \ The build's MARK left its own index mapping's address here; the product's
   \ first MARK builds one from 0.
   0 ADDRESS-CELLS:INDEX-CELL COPY-AT LE:U64! ;


\ The header cells HEADER-CELLS gives a code address.
: LIST-HEADER ( -- )
   RBASE-CELL A64ICODE:DCQ,
   UNCGH-CELL A64ICODE:DCQ,
   SIGNAL-ABI:STUB-CELL A64ICODE:DCQ,
   CODE-END-CELL A64ICODE:DCQ, ;


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
   SORT-ROWS
   ['] COPY-ROW EACH-ROW
   HEADER-CELLS
   RUNS,
   LIMAGE-END A64ICODE:LABEL@ A64ICODE:LBL,
   LLIST A64ICODE:LABEL@ A64ICODE:LBL,
   0 LISTED-N !  ['] LIST-ROW EACH-ROW
   LIST,
   LIST-HEADER
   LLIST-END A64ICODE:LABEL@ A64ICODE:LBL, ;


\ A stripped program's code: the start-up, the kernel code the program reaches,
\ the routines and DATA. HEADER-CELLS reads LUNCAUGHT and LSIGH after the cut.
: LINK-STRIPPED ( -- )
   ARM64:OPEN
   A64ICODE:LBL LIMAGE !  A64ICODE:LBL LIMAGE-END !
   A64ICODE:LBL LLIST !  A64ICODE:LBL LLIST-END !
   TEXT-VA  LIMAGE A64ICODE:LABEL@  LIMAGE-END A64ICODE:LABEL@
   LLIST A64ICODE:LABEL@  LLIST-END A64ICODE:LABEL@  A64BOOT:START,
   ARM64:KERNEL
   ROUTINES
   ROOT-CELLS
   LUNCAUGHT A64ICODE:LABEL@ ARM64:ROOT-LABEL
   LSIGH A64ICODE:LABEL@ ARM64:ROOT-LABEL
   ARM64:CUT
   ['] DATA-ADDRESS PLACE-ROUTINES
   DATA-IMAGE ;


public

\ Link the open space's program and write it, signed, to the path; the caller
\ first stores its entry as app-image-core.f START! does. The facts texts the
\ build's body held lie after every file, so no file's literal can name one.
: WRITE ( ptr u8 n -- )
   {: path:ptr pathu:n :}
   GOING
   ENGINE? @ if E-COMP-STATE throw then
   0 CHECK:PLACE-FACTS
   APP-ENTRY:XT-CELL SPACE-AT @ 0= if
      s" link: the build stored no entry in APP-ENTRY:XT-CELL" 74 die
   then
   LINK-STRIPPED
   SIGN-ID:PROG$ path pathu DRV-EMIT-IMAGE ;

;package
