\ link-data.f - a stripped build's DATA: the open space's bytes appended to
\ A64ICODE's code buffer after the routine band, each declared cell given its
\ value at DATA-VA, then the DATA list, the offset from DATA-VA of every cell
\ holding a code address, which start-up adds the load offset to. Words visible
\ only inside LINKER.

require src/lib/le.f
require src/habu/layout.f
require src/habu/address-cells.f
require src/arch/arm64/icode.f
require src/habu/linker.f
require src/habu/link-band.f

package LINKER
private

\ Each part's byte offset in A64ICODE's code buffer and its length in bytes or
\ its count of u64 offsets, for the boot.
variable DATA-IMAGE-AT
variable DATA-IMAGE-LEN
variable DATA-LIST-AT
variable DATA-LIST-N


\ The open space's bytes, [BASE, DP).
: SPACE-DATA ( -- ptr u8 n )
   NULL-PTR BYTE-VIEW BASE @ +  DP-CELL HOST@ BASE @ - ;

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

: COPY-AT ( n -- ptr u8 ) A64ICODE:CODE DATA-IMAGE-AT @ + swap + ;


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
   off CELL + DATA-IMAGE-LEN @ > if
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
   row ROW-OFF A64ICODE:DCQ,
   1 DATA-LIST-N +! ;


\ After PLACE-ROUTINES: the space's bytes at an 8-byte boundary, then the DATA
\ list at the next one.
: DATA-IMAGE ( -- )
   PAD8
   A64ICODE:ASM-LEN DATA-IMAGE-AT !
   SPACE-DATA {: at:ptr u:n :}
   u DATA-IMAGE-LEN !
   at u A64ICODE:BYTES,
   ['] COPY-ROW EACH-ROW
   PAD8
   A64ICODE:ASM-LEN DATA-LIST-AT !
   0 DATA-LIST-N !
   ['] LIST-ROW EACH-ROW ;


\ A stripped program's routines and DATA, after the kernel ARM64:KERNEL wrote.
: LINK-STRIPPED ( -- )
   ROUTINES
   ROOT-CELLS
   ['] DATA-ADDRESS PLACE-ROUTINES
   DATA-IMAGE ;

;package
