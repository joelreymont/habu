\ Host-side snapshot framing. The cell grammar is IMAGE-CELLS; the emitted
\ startup projects these extent/padding checks before it touches live DATA.
require src/habu/address-cells.f
require src/habu/image-cells.f

package SNAPSHOT-DATA
public

: VERSION? ( n -- bool )
   dup ADDRESS-CELLS:SNAPSHOT-VERSION = swap SNAP-FORMAT-VERSION = or ;

\ Zero means malformed framing; a snapshot always has a nonempty DATA header.
: EXTENT ( ptr u8 n n -- n ) {: src:ptr size:n version:n :}
   version VERSION? 0= size 0 <= or size DATA-SIZE > or if 0 exit then
   version ADDRESS-CELLS:SNAPSHOT-VERSION = if size exit then
   size 16 < if 0 exit then
   src CELL-VIEW @ {: bytes:n :}
   bytes 0 <= bytes DATA-SIZE > or if 0 else bytes then ;

private
: ZERO ( ptr u8 n -- ) {: dst:ptr size:n :}
   size CELL / 0 ?do 0 dst i cells + CELL-VIEW ! loop
   size size CELL / cells ?do 0 dst i + c! loop ;

: PAD? ( ptr u8 n -- bool ) {: src:ptr size:n :}
   size 0 < size PROT-PAGE-MAX >= or if false exit then
   size 0 ?do src i + c@ 0<> if false unloop exit then loop true ;

public
\ Destination is scratch. Answer used input bytes excluding sparse alignment
\ padding, or zero on refusal. Even absent cells replace the destination's old
\ bytes. The caller checks address-cell/WID headers before publishing DATA.
: READ ( ptr u8 n n ptr u8 n -- n )
   {: src:ptr size:n version:n dst:ptr room:n :}
   src size version EXTENT dup 0= swap room <> or if 0 exit then
   version ADDRESS-CELLS:SNAPSHOT-VERSION = if
      src dst room BYTE-COPY size exit
   then
   dst room ZERO
   src 8 + size 8 - dst room IMAGE-CELLS:READ {: used:n :}
   used 0= if 0 exit then
   used 8 + {: end:n :}
   src end + size end - PAD? if end else 0 then ;

;package
