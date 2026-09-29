\ sites.f - the one reader of the live region's recorded sites.
\
\ A site is an instruction in the live code region that a later pass rewrites or
\ resolves: a call, or the first instruction of an address literal. It is
\ recorded where the code is built (src/habu/layout.f SNAP-RELOC) and never found
\ again by decoding region bytes, because compiled code may carry inline data.
\ This file answers one question about that record - which sites start inside a
\ span of region byte offsets, and of which kind - in ascending offset order,
\ from whichever of the two representations the running target keeps:
\
\ - ARM64 keeps two bitmaps, one bit per four-byte region word. The call map
\   holds region-to-text calls only: the compilers record a call whose callee
\   lies outside the region (habu2.f EMIT-CEMITBL, src/compiler/native/publish.f),
\   and nothing records a call from one region address to another. The address
\   map holds the first MOVZ of every address carrier. BITMAP-EACH reads them.
\ - x86-64 keeps rows over the same bytes, because its sites are byte offsets: a
\   u64 count, then offset u32 and kind u8 rows in strictly ascending offset
\   order. ROWS-EACH reads them.
\
\ Both arms are public and take the band - its first byte, SNAP-RELOC's
\ CALLMAP-OFF - as bytes, so a caller can hand either one a band that is not the
\ engine's own; test/sites.f does. EACH-IN-SPAN is the entry the rest of the
\ tree calls: it picks the arm by target and hands it the engine's band.
\
\ A span is [off, off+len) and a site is inside it when its first byte is. Every
\ refusal throws SNAP-RELOC:SITE-RC and precedes the first yield: a span that
\ does not lie inside [0, REGION], on both arms, because the bitmap arm would
\ read past both maps; and on the row arm a count above SITE-CAP, a row at or
\ after REGION, a row of no known kind, and a row not above the one before it.

require lib/prelude.f
require lib/le.f
require src/habu/layout.f

package SITES

private

SNAP-RELOC:ADDRMAP-OFF SNAP-RELOC:CALLMAP-OFF - constant ADDR-MAP-AT
SNAP-RELOC:SITE-ROWS-OFF SNAP-RELOC:SITE-N-CELL - constant ROWS-AT
4 constant WORD-BYTES                  \ one bitmap bit per ARM64 instruction word

: REFUSE ( -- )
   SNAP-RELOC:SITE-RC throw ;

\ Compared without forming off+len, which a large len would wrap.
: SPAN-CK ( n n -- ) {: off:n len:n :}
   off 0 < len 0 < or if REFUSE then
   off REGION > if REFUSE then
   len REGION off - > if REFUSE then ;

\ ---- the bitmap arm -----------------------------------------------------------
\ The bit of the region word at `off`: map byte off >> 5, bit (off >> 2) & 7, the
\ indexing habu1.f BCALLMAPSET and BADDRMAPSET write with.
: BIT? ( ptr u8 n -- bool ) {: map:ptr off:n :}
   map off 5 rshift + c@  off 2 rshift 7 and rshift  1 and 0<> ;

\ One word, both maps. A word is never both a call and a chain start; were it
\ both, the call would come first.
: WORD-EACH ( ptr u8 n [ n n -- ] -- ) {: band:ptr w:n q :}
   band w BIT? if w SNAP-RELOC:SITE-CALL q execute then
   band ADDR-MAP-AT + w BIT? if w SNAP-RELOC:SITE-ADDR q execute then ;

\ ---- the row arm --------------------------------------------------------------
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

\ The engine's own band. DATA is the running task's, the one the engine's
\ writers address.
: BAND ( -- ptr u8 )
   data-base SNAP-RELOC:CALLMAP-OFF + ;

public

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

: EACH-IN-SPAN ( n n [ n n -- ] -- ) {: off:n len:n q :}
   HB-TARGET-LINUX-X86-64? if BAND off len q ROWS-EACH exit then
   BAND off len q BITMAP-EACH ;

;package
