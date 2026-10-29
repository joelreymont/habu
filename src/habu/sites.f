\ sites.f - the one reader of the live region's recorded sites: a call, or the
\ first instruction of an address literal, that a later pass rewrites. Sites are
\ recorded where code is built (layout.f SNAP-RELOC), never found by decoding,
\ because compiled code may carry inline data. EACH-IN-SPAN yields the sites
\ whose first byte lies in [off, off+len), ascending, from the target's form.

require src/lib/prelude.f
require src/lib/le.f
require src/habu/layout.f

package SITES

private

SNAP-RELOC:ADDRMAP-OFF SNAP-RELOC:CALLMAP-OFF - constant ADDR-MAP-AT
SNAP-RELOC:SITE-ROWS-OFF SNAP-RELOC:SITE-N-CELL - constant ROWS-AT
4 constant WORD-BYTES                  \ one bitmap bit per ARM64 instruction word

: REFUSE ( -- )
   SNAP-RELOC:SITE-RC throw ;

\ Every refusal throws SNAP-RELOC:SITE-RC before the first yield: on both arms a
\ span outside [0, REGION] (the bitmap arm would read past both maps); on the row
\ arm a count above SITE-CAP, a row at or after REGION, a row of no known kind,
\ and a row not above the one before it.
\ Compared without forming off+len, which a large len would wrap.
: SPAN-CK ( n n -- ) {: off:n len:n :}
   off 0 < len 0 < or if REFUSE then
   off REGION > if REFUSE then
   len REGION off - > if REFUSE then ;

\ ARM64 keeps two bitmaps, one bit per region word. The call map holds
\ region-to-text calls only (habu2.f EMIT-CEMITBL, src/compiler/native/publish.f);
\ nothing records a region-to-region call. The address map holds the first MOVZ
\ of every address carrier.
\ The bit of the region word at `off`: map byte off >> 5, bit (off >> 2) & 7, the
\ indexing habu1.f BCALLMAPSET and BADDRMAPSET write with.
: BIT? ( ptr u8 n -- bool ) {: map:ptr off:n :}
   map off 5 rshift + c@  off 2 rshift 7 and rshift  1 and 0<> ;

\ One word, both maps. A word is never both a call and a chain start; were it
\ both, the call would come first.
: WORD-EACH ( ptr u8 n [ n n -- ] -- ) {: band:ptr w:n q :}
   band w BIT? if w SNAP-RELOC:SITE-CALL q execute then
   band ADDR-MAP-AT + w BIT? if w SNAP-RELOC:SITE-ADDR q execute then ;

\ x86-64 keeps rows, because its sites are byte offsets: a u64 count, then
\ offset u32 and kind u8 rows in strictly ascending offset order.
: ROW ( ptr u8 n -- ptr u8 ) {: band:ptr k:n :}
   band ROWS-AT + k SNAP-RELOC:SITE-ROW-BYTES * + ;

: ROW-KIND@ ( ptr u8 -- n )
   SNAP-RELOC:SITE-KIND-OFF + c@ ;

: KIND? ( n -- bool ) {: kind:n :}
   kind SNAP-RELOC:SITE-CALL =  kind SNAP-RELOC:SITE-ADDR = or ;

: COUNT-CK ( ptr u8 -- n )
   LE:U64@ {: n:n :}
   n 0 <  n SNAP-RELOC:SITE-CAP > or if REFUSE then
   n ;

\ Row k against the offset of the row before it; answers row k's offset.
: ROW-CK ( ptr u8 n n -- n ) {: band:ptr k:n prev:n :}
   band k ROW {: r:ptr :}
   r LE:U32@ {: off:n :}
   off REGION >= if REFUSE then
   off prev <= if REFUSE then
   r ROW-KIND@ KIND? 0= if REFUSE then
   off ;

\ The whole band, before anything is yielded; answers its row count.
: ROWS-CK ( ptr u8 -- n ) {: band:ptr :}
   band COUNT-CK {: n:n :}
   -1  n 0 ?do band i rot ROW-CK loop  drop
   n ;

: ROW-YIELD ( ptr u8 n n n [ n n -- ] -- ) {: band:ptr k:n lo:n hi:n q :}
   band k ROW {: r:ptr :}
   r LE:U32@ {: off:n :}
   off lo < off hi >= or if exit then
   off r ROW-KIND@ q execute ;

\ The engine's own band, from SNAP-RELOC's CALLMAP-OFF. DATA is the running
\ task's, the one the engine's writers address.
: BAND ( -- ptr u8 )
   data-base SNAP-RELOC:CALLMAP-OFF + ;

\ The two ARM64 bitmaps, the address map ADDR-MAP-AT bytes after the call map.
: BITMAP-EACH ( ptr u8 n n [ n n -- ] -- ) {: band:ptr off:n len:n q :}
   off len SPAN-CK
   off WORD-BYTES 1 - + WORD-BYTES 1 - invert and {: first:n :}
   off len + first - WORD-BYTES 1 - + WORD-BYTES / 0 ?do
      band first i WORD-BYTES * + q WORD-EACH
   loop ;

\ The x86-64 rows: validated whole, then yielded in their ascending order.
\ Every call validates and then walks the whole band, O(n) in the row count at
\ SITE-N-CELL, up to SITE-CAP (419,428) rows, whatever the span. The ascending
\ invariant permits a later bounded walk, from the first row at or above `off`
\ to the first at or above off+len, without changing its stack effect.
: ROWS-EACH ( ptr u8 n n [ n n -- ] -- ) {: band:ptr off:n len:n q :}
   off len SPAN-CK
   band ROWS-CK 0 ?do
      band i off off len + q ROW-YIELD
   loop ;

public

: EACH-IN-SPAN ( n n [ n n -- ] -- ) {: off:n len:n q :}
   HB-TARGET-LINUX-X86-64? if BAND off len q ROWS-EACH exit then
   BAND off len q BITMAP-EACH ;

;package
