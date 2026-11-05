\ link-data.f - a stripped build's DATA: the open space's bytes appended to
\ A64ICODE's code buffer after the routine band, each declared cell given its
\ value at DATA-VA, then the DATA list, the offset from DATA-VA of every cell
\ holding a code address, which start-up adds the load offset to. The link
\ also writes the header cells HEADER-CELLS names and lists the code ones.
\ Words visible only inside LINKER.

require src/lib/le.f
require src/habu/layout.f
require src/habu/boot-cells.f
require src/habu/wl.f
require src/habu/address-cells.f
require src/arch/arm64/icode.f
require src/arch/arm64/link-kernel.f
require src/arch/arm64/boot.f
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
: LABEL-OFFSET ( ptr n -- n )
   A64ICODE:LABEL@ {: l:label :}
   l A64ICODE:LBL-BOUND? 0= if
      s" link: a label the link reads is unbound" 74 die
   then
   l LABEL>N A64ICODE:LBLP @ 4 * ;


\ The open space's bytes, [BASE, DP).
: SPACE-DATA ( -- ptr u8 n )
   NULL-PTR BYTE-VIEW BASE @ +  DP-CELL HOST@ BASE @ - ;

\ BYTES, pads the image to a word, so its length is the space's, not the
\ distance between its labels.
: DATA-IMAGE-AT ( -- n ) LIMAGE LABEL-OFFSET ;
: DATA-IMAGE-LEN ( -- n ) SPACE-DATA nip ;
: DATA-LIST-AT ( -- n ) LLIST LABEL-OFFSET ;
: DATA-LIST-N ( -- n ) LLIST-END LABEL-OFFSET DATA-LIST-AT - 8 / ;

: ROWS ( -- ptr n n ) SPACE-DATA ADDRESS-CELLS:DATA-SPAN ;


\ A host address in the open space at its DATA-VA address: the resolver code's
\ data address sites get. Native `.` ends its line.
: DATA-ADDRESS ( n -- n )
   {: v:n :}
   v BASE @ - {: off:n :}
   off 0 >=  off DATA-SIZE < and if DATA-VA VA>N off + exit then
   s" link: a DATA pointer outside the build's space: " type v .
   s" link: a DATA pointer outside the build's space" 74 die ;


\ A row's cell offset from the space's start, and its kind (layout.f).
: ROW-OFF ( n -- n ) SNAP-RELOC:XTCELL-OFF-MASK and ;
: SCRATCH-ROW? ( n -- bool ) SNAP-RELOC:XTCELL-SCRATCH-TAG and 0<> ;
: DATA-ROW? ( n -- bool ) SNAP-RELOC:XTCELL-DATA-TAG and 0<> ;
: XT-ROW? ( n -- bool )
   SNAP-RELOC:XTCELL-DATA-TAG SNAP-RELOC:XTCELL-SCRATCH-TAG or and 0= ;

: CELL-VALUE ( n -- n ) ROW-OFF SPACE-AT @ ;


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

: COPY-AT ( n -- ptr u8 ) A64ICODE:CODE DATA-IMAGE-AT + swap + ;


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


\ A cell holding 0 holds no code address and is not listed.
: LIST-ROW ( n -- )
   {: row:n :}
   row XT-ROW? 0= if exit then
   row CELL-VALUE 0= if exit then
   row ROW-OFF A64ICODE:DCQ, ;


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
   TEXT-VA DATA-IMAGE-AT +  CODE-END-CELL HEADER! ;

\ The header cells HEADER-CELLS gives a code address.
: LIST-HEADER ( -- )
   RBASE-CELL A64ICODE:DCQ,
   UNCGH-CELL A64ICODE:DCQ,
   SIGNAL-ABI:STUB-CELL A64ICODE:DCQ,
   CODE-END-CELL A64ICODE:DCQ, ;


\ After PLACE-ROUTINES: the space's bytes at an 8-byte boundary with the header
\ cells written, then the DATA list at the next one, the code header cells last.
: DATA-IMAGE ( -- )
   PAD8
   LIMAGE A64ICODE:LABEL@ A64ICODE:LBL,
   SPACE-DATA A64ICODE:BYTES,
   LIMAGE-END A64ICODE:LABEL@ A64ICODE:LBL,
   ['] COPY-ROW EACH-ROW
   HEADER-CELLS
   PAD8
   LLIST A64ICODE:LABEL@ A64ICODE:LBL,
   ['] LIST-ROW EACH-ROW
   LIST-HEADER
   LLIST-END A64ICODE:LABEL@ A64ICODE:LBL, ;


\ A stripped program's code: the start-up, the kernel, the routines and DATA.
: LINK-STRIPPED ( -- )
   ARM64:OPEN
   A64ICODE:LBL LIMAGE !  A64ICODE:LBL LIMAGE-END !
   A64ICODE:LBL LLIST !  A64ICODE:LBL LLIST-END !
   TEXT-VA  LIMAGE A64ICODE:LABEL@  LIMAGE-END A64ICODE:LABEL@
   LLIST A64ICODE:LABEL@  LLIST-END A64ICODE:LABEL@  A64BOOT:START,
   ARM64:KERNEL
   ROUTINES
   ROOT-CELLS
   ['] DATA-ADDRESS PLACE-ROUTINES
   DATA-IMAGE ;

;package
