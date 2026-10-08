\ sites-child.f - SITES, the one reader of the live region's recorded sites,
\ driven over both of the band's representations (src/habu/layout.f SNAP-RELOC).
\
\ Both arms take their band as bytes, so this suite hands each a scratch band of
\ the real size, [CALLMAP-OFF, ADDRMAP-END), writes sites into it in the shape
\ layout.f declares, and records what the arm yields. The ARM64 arm is also
\ driven through EACH-IN-SPAN over the engine's own maps, marked by the real
\ `callmap-set` and `addrmap-set` and cleared by the real `reloc-maps-clear`, so
\ the bitmap this suite writes and the one the engine writes cannot disagree
\ unnoticed. HB-TARGET-LINUX-X86-64? is false on an ARM64 host, so there
\ EACH-IN-SPAN's row dispatch is not reached; ROWS-EACH is driven directly.
\
\ What a reader can get wrong, and the case that sees it:
\ - a bit's byte index or bit number: words 0, 3, 7, 8, 9 and the region's last;
\ - the address map's base, a swapped kind, or calls and addresses yielded as two
\   runs instead of one ascending merge: the kinds interleave across a map byte;
\ - a span edge: sites before, at, inside and at the end of a span, and a span
\   that starts inside a word;
\ - a span outside the region, which on the bitmap arm reads past both maps;
\ - a row's byte order, stride or kind position: an offset with four distinct
\   bytes among several rows of both kinds;
\ - the capacity: SITE-CAP rows read to the band's last row, one more refused,
\   and a count with its top bit set refused;
\ - the region: a row at REGION - 1 read, a row at REGION refused;
\ - a row not above the one before it, or of no known kind: refused;
\ - a refusal found after rows inside the span: nothing is yielded.

require lib/test.f
require lib/le.f
require lib/memory.f
require src/habu/layout.f
require src/habu/sites.f

package SITES-TEST

private

\ ---- the scratch band ---------------------------------------------------------
SNAP-RELOC:ADDRMAP-END SNAP-RELOC:CALLMAP-OFF - constant BAND-BYTES
SNAP-RELOC:ADDRMAP-OFF SNAP-RELOC:CALLMAP-OFF - constant ADDR-MAP-AT
SNAP-RELOC:SITE-ROWS-OFF SNAP-RELOC:SITE-N-CELL - constant ROWS-AT
4 constant WORD-BYTES                  \ one bitmap bit per ARM64 instruction word

PTR-VARIABLE BAND-P

: BAND ( -- ptr u8 )
   BAND-P @ ;

: BAND-ALLOC ( -- )
   BAND-BYTES MEM:BYTES-ALLOC-LEN MEM:ALLOC-BYTES drop BAND-P ! ;

: CLEAR ( -- )
   BAND {: b:ptr :}
   BAND-BYTES 0 ?do 0 b i + c! loop ;

\ ---- writing the two representations -----------------------------------------
\ A bitmap bit is the region word at `off`: map byte off >> 5, bit (off >> 2) & 7.
: MARK ( n n -- ) {: map:n off:n :}
   BAND map + off 5 rshift + {: p:ptr :}
   p c@  1 off 2 rshift 7 and lshift  or  p c! ;

: CALL-BIT ( n -- )
   0 swap MARK ;

: ADDR-BIT ( n -- )
   ADDR-MAP-AT swap MARK ;

: COUNT! ( n -- )
   BAND LE:U64! ;

: ROW! ( n n n -- ) {: k:n off:n kind:n :}
   BAND ROWS-AT + k SNAP-RELOC:SITE-ROW-BYTES * + {: r:ptr :}
   off r LE:U32!
   kind r SNAP-RELOC:SITE-KIND-OFF + c! ;

\ ---- recording what an arm yields ---------------------------------------------
\ Every yield is counted; the first GOT-CAP are kept for exact comparison, and the
\ last is kept whatever the count, for the capacity case.
64 constant GOT-CAP
GOT-CAP TYPED-BUFFER GOT-OFF n
GOT-CAP TYPED-BUFFER GOT-KIND n
variable GOT-N
variable LAST-OFF
variable LAST-KIND

: GOT ( n n -- ) {: off:n kind:n :}
   GOT-N @ {: k:n :}
   k GOT-CAP < if
      off k GOT-OFF !
      kind k GOT-KIND !
   then
   off LAST-OFF !
   kind LAST-KIND !
   k 1+ GOT-N ! ;

: YIELD= ( n n n -- ) {: k:n off:n kind:n :}
   k GOT-OFF @ off T=
   k GOT-KIND @ kind T= ;

: BITMAP ( n n -- ) {: off:n len:n :}
   0 GOT-N !
   BAND off len [: GOT ;] SITES:BITMAP-EACH ;

: ROWS ( n n -- ) {: off:n len:n :}
   0 GOT-N !
   BAND off len [: GOT ;] SITES:ROWS-EACH ;

: CALL-KIND ( -- n ) SNAP-RELOC:SITE-CALL ;
: ADDR-KIND ( -- n ) SNAP-RELOC:SITE-ADDR ;

\ ---- the bitmap arm -----------------------------------------------------------
\ Calls at words 0, 7 and 9, addresses at words 3 and 8 and at the region's last
\ word: the kinds alternate, and words 7 and 8 sit either side of a map byte.
: BITMAP-FILL ( -- )
   CLEAR
   0 CALL-BIT  12 ADDR-BIT  28 CALL-BIT  32 ADDR-BIT  36 CALL-BIT
   REGION WORD-BYTES - ADDR-BIT ;

: BITMAP-EXACT ( -- )
   s" the bitmap arm yields every site of both maps in ascending order" T-LABEL
   BITMAP-FILL
   0 REGION BITMAP
   GOT-N @ 6 T=
   0 0 CALL-KIND YIELD=  1 12 ADDR-KIND YIELD=  2 28 CALL-KIND YIELD=
   3 32 ADDR-KIND YIELD=  4 36 CALL-KIND YIELD=
   5 REGION WORD-BYTES - ADDR-KIND YIELD= ;

: BITMAP-SPAN ( -- )
   s" the bitmap arm yields the words that start inside the span" T-LABEL
   BITMAP-FILL
   12 24 BITMAP
   GOT-N @ 3 T=
   0 12 ADDR-KIND YIELD=  1 28 CALL-KIND YIELD=  2 32 ADDR-KIND YIELD=
   s" a span starting inside a word begins at the next word" T-LABEL
   13 24 BITMAP
   GOT-N @ 3 T=
   0 28 CALL-KIND YIELD=  1 32 ADDR-KIND YIELD=  2 36 CALL-KIND YIELD=
   s" an empty span yields nothing, at the region's end too" T-LABEL
   12 0 BITMAP  GOT-N @ 0 T=
   REGION 0 BITMAP  GOT-N @ 0 T= ;

: BITMAP-BELOW ( -- )
   0 WORD-BYTES - 8 BITMAP ;

: BITMAP-PAST ( -- )
   REGION WORD-BYTES - 8 BITMAP ;

: BITMAP-AFTER ( -- )
   REGION WORD-BYTES + 0 BITMAP ;

: BITMAP-NEGATIVE ( -- )
   0 -1 BITMAP ;

: BITMAP-REFUSED ( -- )
   BITMAP-FILL
   s" a span starting below the region is refused" T-LABEL
   [: BITMAP-BELOW ;] SNAP-RELOC:SITE-RC TTHROWSQ  GOT-N @ 0 T=
   s" a span reaching past the region is refused" T-LABEL
   [: BITMAP-PAST ;] SNAP-RELOC:SITE-RC TTHROWSQ  GOT-N @ 0 T=
   s" a span starting past the region is refused" T-LABEL
   [: BITMAP-AFTER ;] SNAP-RELOC:SITE-RC TTHROWSQ  GOT-N @ 0 T=
   s" a negative length is refused" T-LABEL
   [: BITMAP-NEGATIVE ;] SNAP-RELOC:SITE-RC TTHROWSQ  GOT-N @ 0 T= ;

\ ---- the row arm --------------------------------------------------------------
\ Byte offsets, as x86 sites are; $01D2E3F4 has four distinct bytes.
$01D2E3F4 constant WIDE

: ROWS-FILL ( -- )
   CLEAR
   4 COUNT!
   0 1 ADDR-KIND ROW!  1 6 CALL-KIND ROW!  2 WIDE ADDR-KIND ROW!
   3 REGION 1 - CALL-KIND ROW! ;

: ROWS-EXACT ( -- )
   s" the row arm yields every row in order with its kind" T-LABEL
   ROWS-FILL
   0 REGION ROWS
   GOT-N @ 4 T=
   0 1 ADDR-KIND YIELD=  1 6 CALL-KIND YIELD=  2 WIDE ADDR-KIND YIELD=
   3 REGION 1 - CALL-KIND YIELD= ;

: ROWS-SPAN ( -- )
   s" the row arm yields the rows inside the span" T-LABEL
   ROWS-FILL
   6 WIDE 6 - ROWS
   GOT-N @ 1 T=
   0 6 CALL-KIND YIELD=
   s" the span's last byte is inside it" T-LABEL
   6 WIDE 5 - ROWS
   GOT-N @ 2 T=
   0 6 CALL-KIND YIELD=  1 WIDE ADDR-KIND YIELD= ;

\ Every row the band holds: offsets 64 apart, kinds alternating.
: ROWS-FULL ( -- )
   CLEAR
   SNAP-RELOC:SITE-CAP COUNT!
   SNAP-RELOC:SITE-CAP 0 ?do
      i  i 64 *  i 1 and 0<> if ADDR-KIND else CALL-KIND then  ROW!
   loop ;

: ROWS-OVER ( -- )
   SNAP-RELOC:SITE-CAP 1+ COUNT!
   0 REGION ROWS ;

: ROWS-NEGATIVE ( -- )
   -1 COUNT!
   0 REGION ROWS ;

: ROWS-CAPACITY ( -- )
   s" a full band is read to its last row" T-LABEL
   ROWS-FULL
   0 REGION ROWS
   GOT-N @ SNAP-RELOC:SITE-CAP T=
   LAST-OFF @ SNAP-RELOC:SITE-CAP 1 - 64 * T=
   LAST-KIND @
   SNAP-RELOC:SITE-CAP 1 - 1 and 0<> if ADDR-KIND else CALL-KIND then T=
   s" the last row ends inside the band" T-LABEL
   ROWS-AT SNAP-RELOC:SITE-CAP SNAP-RELOC:SITE-ROW-BYTES * + BAND-BYTES <= TTRUE
   s" a span inside a full band" T-LABEL
   320 128 ROWS
   GOT-N @ 2 T=
   0 320 ADDR-KIND YIELD=  1 384 CALL-KIND YIELD=
   s" a count above SITE-CAP is refused" T-LABEL
   [: ROWS-OVER ;] SNAP-RELOC:SITE-RC TTHROWSQ  GOT-N @ 0 T=
   s" a count with its top bit set is refused" T-LABEL
   [: ROWS-NEGATIVE ;] SNAP-RELOC:SITE-RC TTHROWSQ  GOT-N @ 0 T= ;

: ROWS-ALL ( -- )
   0 REGION ROWS ;

: ROWS-HEAD ( -- )
   0 16 ROWS ;

: ROWS-PAST ( -- )
   REGION WORD-BYTES - 8 ROWS ;

\ Two rows in span ahead of the one that is wrong, so a reader that yields as it
\ validates is seen to.
: BAD-THIRD ( n n -- ) {: off:n kind:n :}
   CLEAR
   3 COUNT!
   0 4 CALL-KIND ROW!  1 8 ADDR-KIND ROW!  2 off kind ROW! ;

: ROWS-REFUSED ( -- )
   s" a row at REGION is refused before any row is yielded" T-LABEL
   REGION ADDR-KIND BAD-THIRD
   [: ROWS-HEAD ;] SNAP-RELOC:SITE-RC TTHROWSQ  GOT-N @ 0 T=
   s" a row below the one before it is refused" T-LABEL
   2 CALL-KIND BAD-THIRD
   [: ROWS-ALL ;] SNAP-RELOC:SITE-RC TTHROWSQ  GOT-N @ 0 T=
   s" a second row at one offset is refused" T-LABEL
   8 CALL-KIND BAD-THIRD
   [: ROWS-ALL ;] SNAP-RELOC:SITE-RC TTHROWSQ  GOT-N @ 0 T=
   s" a row of no known kind is refused" T-LABEL
   12 0 BAD-THIRD
   [: ROWS-ALL ;] SNAP-RELOC:SITE-RC TTHROWSQ  GOT-N @ 0 T=
   12 ADDR-KIND CALL-KIND max 1+ BAD-THIRD
   [: ROWS-ALL ;] SNAP-RELOC:SITE-RC TTHROWSQ  GOT-N @ 0 T=
   s" a span reaching past the region is refused on the row arm too" T-LABEL
   ROWS-FILL
   [: ROWS-PAST ;] SNAP-RELOC:SITE-RC TTHROWSQ  GOT-N @ 0 T= ;

\ ---- EACH-IN-SPAN over the engine's own band -----------------------------------
\ The words above the code pointer are unclaimed until something publishes there,
\ so the suite may mark and clear them. The three primitives' rows are private
\ to their owners (NPUB, CODE-RECLAIM), so this suite runs in test/sites.f's
\ window and calls the owners' checked words test/reloc-window-prepare.f defines
\ there, as src/compiler/native/publish.f and src/habu/xref.f call them from
\ their owners; each takes an address the checked words below compute.

64 constant SPAN-BYTES

: LIVE ( -- n )
   cp@ dbase@ - ;

: LIVE-EACH ( n -- ) {: len:n :}
   0 GOT-N !
   LIVE len [: GOT ;] SITES:EACH-IN-SPAN ;

: LIVE-CASE ( -- )
   s" EACH-IN-SPAN reads what callmap-set and addrmap-set recorded" T-LABEL
   cp@ SPAN-BYTES WORD-BYTES + CODE-RECLAIM:MAPS-CLEAR
   cp@ 32 + NPUB:CALLMAP-MARK
   cp@ NPUB:ADDRMAP-MARK
   cp@ 28 + NPUB:ADDRMAP-MARK
   cp@ 12 + NPUB:CALLMAP-MARK
   cp@ SPAN-BYTES + NPUB:ADDRMAP-MARK
   SPAN-BYTES LIVE-EACH
   GOT-N @ 4 T=
   0 LIVE ADDR-KIND YIELD=  1 LIVE 12 + CALL-KIND YIELD=
   2 LIVE 28 + ADDR-KIND YIELD=
   3 LIVE 32 + CALL-KIND YIELD=
   s" the site one word past the span is its own" T-LABEL
   SPAN-BYTES WORD-BYTES + LIVE-EACH
   GOT-N @ 5 T=
   4 LIVE SPAN-BYTES + ADDR-KIND YIELD=
   s" reloc-maps-clear leaves EACH-IN-SPAN nothing to yield" T-LABEL
   cp@ SPAN-BYTES WORD-BYTES + CODE-RECLAIM:MAPS-CLEAR
   SPAN-BYTES WORD-BYTES + LIVE-EACH
   GOT-N @ 0 T= ;

public

: RUN ( -- )
   T-RESET
   BAND-ALLOC
   BITMAP-EXACT
   BITMAP-SPAN
   BITMAP-REFUSED
   ROWS-EXACT
   ROWS-SPAN
   ROWS-CAPACITY
   ROWS-REFUSED
   LIVE-CASE
   T-REPORT
   s" sites: ok" type cr ;

;package

SITES-TEST:RUN
