\ aot-decl.f - the AOT capture format's storage: every buffer the capture fills,
\ every cap that bounds it, and the code-window budget derived from those caps.

\ In a booted bin/hb the icode.f require loads the section budget AOT-SECTION-CAP,
\ which the engine does not keep.
require src/arch/arm64/icode.f
require src/habu/layout.f
require src/habu/address-carrier.f
require src/habu/code-span.f
require src/habu/cell-grid.f

\ The public tails keep their AOT- prefix: a bare tail can clash with a global
\ word, so a rename checks each.
package AOT-BUF
public

\ A capture must hold any code window the fixed runtime region admits.
CODE-BAND:BYTES constant AOT-BLOB-CAP
\ Capture storage belongs to the build, not the target DATA heap.
DYNAMIC-BUFFER BLOB-STORAGE n
: AOT-BLOB-BUF ( -- ptr u8 )
   AOT-BLOB-CAP CELL / BLOB-STORAGE-RESERVE
   0 BLOB-STORAGE BYTE-VIEW ;
variable AOT-BLOB-LEN
\ REPL snapshots retain the full dictionary and do not use this window.
32768 constant AOT-REC-MAX
\ AOT-REC-BUF holds MAX verbatim 48-byte dictionary records, then MAX compact
\ AOT-CREC-ROW-byte ones.
DYNAMIC-BUFFER REC-STORAGE n
: AOT-REC-BUF ( -- ptr u8 )
   AOT-REC-MAX 48 * AOT-REC-MAX AOT-CREC-ROW * + CELL / REC-STORAGE-RESERVE
   0 REC-STORAGE BYTE-VIEW ;
variable AOT-REC-N
\ A compact row's wid word on a package row, whose first two words are then its
\ public and private wids.
$FFFFFFFF constant PKG-MARK
\ The packed little-endian u32 every row field of this format is.
: W32@ ( ptr u8 -- n )
   {: p:ptr :}
   0 4 0 ?do p i + c@ i 8 * lshift or loop ;
\ A call-site row: three u32 words - blob-off, name-off, and the callee's SCOPE.
\ WID-QUAL says the pooled name is qualified and carries its own scope.
12 constant SITE-ROW
\ The IMAGE's row is the same three words, but its middle word is a TARGET
\ rather than a name. A CALLEE THE IMAGE DOES NOT CARRY KEEPS ITS NAME: such a
\ row sets SITE-NAME-TAG on the target and carries the pool offset in the low
\ bits. THE TWO INDEX KINDS ARE NOT THE SAME NUMBER: a primitive's index is
\ absolute, a payload record's is RELATIVE to wherever the records pass put
\ them, so the row says which kind it carries.
$80000000 constant SITE-NAME-TAG
$40000000 constant SITE-REC-TAG
$C0000000 constant SITE-SHADOW-TAG
$3FFFFFFF constant SITE-TARGET-MASK
$FFFFFFFE constant WID-QUAL

$28000 constant AOT-SITE-MAX
create AOT-SITE-BUF AOT-SITE-MAX SITE-ROW * allot    variable AOT-SITE-N
\ The merge reads and writes this buffer after the capture returns: releasing it
\ inside the capture would emit an empty name pool. A capture tool loads this
\ file before the window opens, so its control record sits BELOW the captured
\ DATA window. A declaration moved above a window open would be released
\ mid-capture.
DYNAMIC-BUFFER AOT-NAMES-STORAGE n
variable AOT-NAMES-LEN
: AOT-NAMES-RESERVE ( n -- ) CELL 1- + CELL / AOT-NAMES-STORAGE-RESERVE ;
\ DATA relocation rows contain a blob offset. Bit 31 selects a raw address
\ cell in a recognised metadata trailer; otherwise it names an address chain.
$80000000 constant AOT-DSITE-CELL
$7FFFFFFF constant AOT-DSITE-OFF-MASK
\ DATA and CODE rows partition the aligned relocation starts in the blob.
\ At most one row can name each four-byte word, including deduplicated defer
\ metadata cells.
AOT-BLOB-CAP 4 / constant AOT-DSITE-MAX
DYNAMIC-BUFFER DSITE-STORAGE n
variable AOT-DSITE-N                            \ packed u32 offsets, DATA then CODE

: AOT-DSITE-RESERVE ( n -- )
   dup 0< over AOT-DSITE-MAX > or if
      s" aot: relocation site count exceeds the code blob bound" 74 die
   then
   1+ 2 / DSITE-STORAGE-RESERVE ;

: AOT-DSITE-BUF ( -- ptr u8 )
   1 AOT-DSITE-RESERVE
   0 DSITE-STORAGE BYTE-VIEW ;
variable AOT-DATA-D0    variable AOT-DATA-SIZE
\ The window's wordlist span, [W0, W0+SPAN). A captured wid is a window-relative
\ coordinate the same way a blob offset is: only wid 0, the global wordlist, means
\ the same thing in two processes. These two cells are the capturing process's
\ own and do not travel. A row holds `wid - W0 + WID-REL-BASE`, wid 0 holds 0.
\ One-based, because the window's FIRST wordlist is one records name and a
\ zero-based offset would spell it the way the global wordlist is spelled. The
\ protected-wordlist rows are offsets too, and zero-based.
1 constant WID-REL-BASE
variable AOT-WID-W0    variable AOT-WID-SPAN
\ A DECLARED ADDRESS CELL IS THE ONE THING THE CONTENT MAY NOT CARRY RAW. An XT
\ cell holds a code address in the BUILDING host; a DATA-pointer cell can hold an
\ address in that host's captured window. Both cells are therefore excluded from
\ the content and listed with their kind.

;package

package AOT-WINDOW
public
\ THE WINDOW'S CONTENT TRAVELS AS CELLS, AND THE SPAN IS A NUMBER: a presence
\ bitmap over the window's cells and one varint per non-zero cell.
\ AOT-DATA-SIZE is a scalar in the artifact, not a section's length. SPAN-CAP
\ bounds only it: it keeps the u32 offsets in the two tables below honest and
\ fails closed on a span no engine could reserve.
$1000000 constant SPAN-CAP
CELL-GRID:CELL-BYTES constant CELL-BYTES
CELL-GRID:CELL-BITS constant CELL-BITS
CELL-GRID:BM-BYTE-SPAN constant BM-BYTE-SPAN
CELL-GRID:GROUP-BYTES constant GROUP-BYTES
CELL-GRID:GROUP-SPAN constant GROUP-SPAN
CELL-GRID:VMAX constant VMAX
\ THE GRID IS THE DATA CELL GRID, NOT THE WINDOW'S OWN. Every declared address
\ cell in the window is eight-byte aligned in DATA - the atomics fault on a
\ misaligned cell - and each must fall on exactly one grid cell, because the
\ capture leaves its bits clear. So a window opens on a cell-aligned base, and a
\ span is rounded UP to a whole number of cells: the bytes it gains are above
\ the captured DP, never read, and zero by construction.
SPAN-CAP BM-BYTE-SPAN / constant BM-CAP
DYNAMIC-BUFFER BM-STORAGE n
: BM-BUF ( -- ptr u8 )
   BM-CAP CELL / BM-STORAGE-RESERVE
   0 BM-STORAGE BYTE-VIEW ;
variable CELL-N                      \ present cells
variable BM-LEN                      \ bytes of bitmap, trailing absent cells dropped
variable CONTENT-END                 \ one past the last present cell: where a merge may pad to

\ The image's second level: a presence bit per GROUP-BYTES bitmap bytes.
\ THE GROUPING IS AN IMAGE ENCODING, NOT THE CAPTURE FORMAT. The capture, the
\ artifact sections and a merge keep the flat bitmap, where appending a window is
\ a concatenation and a cell's bit is at a fixed place.
\ Image form: [groups G][stored bytes S] and then presence map, present groups,
\ values.
BM-CAP GROUP-BYTES / constant GROUP-CAP
GROUP-CAP CELL-BITS / constant PMAP-CAP
PMAP-CAP BM-CAP + constant CBM-CAP
DYNAMIC-BUFFER CBM-STORAGE n
: CBM-BUF ( -- ptr u8 )
   CBM-CAP CELL / CBM-STORAGE-RESERVE
   0 CBM-STORAGE BYTE-VIEW ;
variable CBM-GROUPS                  \ groups the presence map covers
variable CBM-STORED                  \ bytes of stored groups, GROUP-BYTES each

: CBM-PMAP-BYTES ( -- n ) CBM-GROUPS @ CELL-GRID:PMAP-BYTES ;

: CBM-LEN ( -- n ) CBM-PMAP-BYTES CBM-STORED @ + ;

\ A flat bitmap in, the compact form in CBM-BUF. The group cap keeps both halves
\ inside CBM-CAP.
: BM-COMPACT ( ptr u8 n -- ) {: bm:ptr len:n :}
   len CELL-GRID:GROUPS GROUP-CAP > if
      s" aot: the window bitmap exceeds the AOT group map" 74 die then
   bm len CBM-BUF CELL-GRID:COMPACT CBM-STORED ! CBM-GROUPS ! ;

\ Every cell in an admitted window may be present and use VMAX bytes.
SPAN-CAP CELL-BYTES / VMAX * constant VAL-CAP
DYNAMIC-BUFFER VAL-STORAGE n
: VAL-BUF ( -- ptr u8 )
   VAL-CAP CELL / VAL-STORAGE-RESERVE
   0 VAL-STORAGE BYTE-VIEW ;
variable VAL-LEN

: WINDOW-RESET ( -- )
   0 CELL-N !  0 BM-LEN !  0 CONTENT-END !  0 VAL-LEN ! ;

: CELL-VLEN ( n -- n ) CELL-GRID:CELL-VLEN ;

: CELL-V! ( n ptr u8 -- n ) CELL-GRID:CELL-V! ;

: CELL-V@ ( ptr u8 n -- n n ) CELL-GRID:CELL-V@ ;

\ The artifact's aggregate byte budget, checked before copy or emission, is the
\ limit; this single-section ceiling also bounds each allocation request.
8 constant XTOFF-ROW
A64ICODE:AOT-SECTION-CAP XTOFF-ROW / constant XTOFF-MAX
$80000000 constant XTOFF-WINDOW-TAG
$80000000 constant XTOFF-DATA-TAG
$C0000000 constant XTOFF-SCRATCH-TAG
$40000000 constant XTOFF-NAME-TAG
$C0000000 constant XTOFF-KIND-MASK
$7FFFFFFF constant XTOFF-LOC-MASK
$3FFFFFFF constant XTOFF-VALUE-MASK
DYNAMIC-BUFFER XTOFF-STORAGE n
variable XTOFF-N
: XTOFF-RESERVE ( n -- )
   dup 0 < over XTOFF-MAX > or if
      s" aot: address-cell section exceeds its byte budget" 74 die then
   XTOFF-STORAGE-RESERVE ;
: XTOFF-BUF ( -- ptr u8 )
   XTOFF-N @ 1 max XTOFF-RESERVE
   0 XTOFF-STORAGE BYTE-VIEW ;
\ Each row is (location u32, target u32). The location's high bit selects a
\ window-relative offset; otherwise it is a fixed DATA offset. The target's
\ two high bits select CODE (00), named CODE (01), DATA (10) or a scratch DATA
\ cell (11), whose low bits are zero: it boots as null.
\ CODE/DATA low bits are zero for null or the target-window offset plus one.
\ Named CODE carries a nonzero name-pool entry offset plus one.
;package

package AOT-BUF
public

\ CODE-literal relocation table: blob offsets of the movz/movk x9 literals whose
\ value lands in the captured code range [B0,B1). Stored in the DATA-site buffer
\ right after the AOT-DSITE-N DATA offsets, and baked as its own contiguous
\ section.
variable AOT-CSITE-N
variable AOT-CODE-B0
\ NAMED code sites: blob offsets of movz/movk literals whose value is the ENTRY
\ OF A WORD, paired with that word's name in the pool. A capture stores 0 in the
\ lanes, so the baked blob carries no host address.

;package

package AOT-XTSITE
public
16384 constant MAX
create BUF MAX 8 * allot    variable N   \ 8B rows: blob-off u32 + name-off u32
;package

\ THE CODE SPANS OF THE WORDS THE IMAGE SHIPS NO RECORD FOR. A ROW IS (blob
\ offset u32, raw code span u32), in capture order, the same two fields and the
\ same CODE-SPAN encoding a compact record's first two words hold.
package AOT-SPAN
private
\ RESERVED AT FIRST USE, NOT ALLOTTED. A static `create BUF MAX ROW * allot`
\ would spend MAX * ROW bytes of the build host's DATA prefix, the DATA below
\ the captured window, in every process that loads this file.
DYNAMIC-BUFFER STORAGE n
public
AOT-BUF:AOT-REC-MAX constant MAX      \ at most one row per window record
8 constant ROW
variable N
: BUF@ ( -- ptr u8 )
   MAX ROW * CELL 1- + CELL / STORAGE-RESERVE
   0 STORAGE BYTE-VIEW ;
;package

\ THE SHADOW: A SECOND TARGET'S ROUTINES FOR THE WINDOW'S RECORDS. A capture
\ taken with no shadow open leaves all four tables empty.
\
\ FOUR TABLES, AND NO HOST ADDRESS IN ANY OF THEM. CODE is every emission's bytes,
\ each emission once, as its own unplaced routine: a call's field is the zero an
\ unplaced emission writes, a DATA literal holds its window DATA offset, a code
\ literal naming a word holds 0, and a `codeaddr` holds its function's offset in
\ its own emission. A record is named by its SHIPPED ROW, its row in the record
\ table the artifact carries: that table holds only the records the capture
\ ships, so past a stripped private word a window index names the wrong one. A
\ RECORD row is (shipped row or ANON-REC plus the capture record index, emission
\ start in CODE, emission bytes,
\ entry offset in it): a `does>` definer and its companion are two rows over one
\ emission, the companion entering where the clause function starts. A SITE row
\ is (byte offset in CODE, kind, target); a target is a shipped record tagged
\ SITE-REC-TAG, an anonymous shadow row tagged SITE-SHADOW-TAG, or a word of the
\ engine's own prefix tagged SITE-NAME-TAG. An XT row is (address-cell row,
\ shipped row or SITE-SHADOW-TAG plus anonymous shadow row): an address cell
\ whose CODE target is a shadow routine's entry, keyed by
\ the record and not by the ARM64 blob offset the address-cell row itself
\ carries.
package AOT-SHADOW
private
DYNAMIC-BUFFER REC-STORAGE n
DYNAMIC-BUFFER CODE-STORAGE n
DYNAMIC-BUFFER SITE-STORAGE n
DYNAMIC-BUFFER XT-STORAGE n

: CAP-DIE ( -- )
   s" aot: a shadow table exceeds its format bound" 74 die ;

\ Room for `n` bytes of a table whose bound is `cap` bytes, in whole cells.
: CELLS-FOR ( n n -- n ) {: n:n cap:n :}
   n 0 < n cap > or if CAP-DIE then
   n CELL 1- + CELL / 1 max ;

public
16 constant REC-ROW
12 constant SITE-ROW
8 constant XT-ROW
$80000000 constant ANON-REC
$FFFFFFFF constant RETIRED-REC
\ An emission is code the window's own region held, so the blob's bound is its
\ bound; reached routines and shipped aliases share the record-table bound;
\ a site is a call or a ten-byte MOVABS,
\ so a byte can start at most one in five; one XT row per address-cell row.
AOT-BUF:AOT-BLOB-CAP constant CODE-CAP
AOT-BUF:AOT-REC-MAX constant REC-MAX
CODE-CAP 5 / constant SITE-MAX
AOT-WINDOW:XTOFF-MAX constant XT-MAX

\ The site kinds. CALL and TAIL hold a rel32 to their target, CODE a MOVABS of
\ the target's entry, DATA a MOVABS of a window DATA offset, FUN a MOVABS of a
\ function's offset in the site's own emission, DCELL a defer trailer cell.
1 constant CALL
2 constant TAIL
3 constant DATA
4 constant CODE
5 constant FUN
6 constant DCELL

variable REC-N
variable CODE-LEN
variable SITE-N
variable XT-N

: REC-RESERVE ( n -- ) REC-ROW * REC-MAX REC-ROW * CELLS-FOR REC-STORAGE-RESERVE ;
: CODE-RESERVE ( n -- ) CODE-CAP CELLS-FOR CODE-STORAGE-RESERVE ;
: SITE-RESERVE ( n -- ) SITE-ROW * SITE-MAX SITE-ROW * CELLS-FOR SITE-STORAGE-RESERVE ;
: XT-RESERVE ( n -- ) XT-ROW * XT-MAX XT-ROW * CELLS-FOR XT-STORAGE-RESERVE ;

: REC-BUF@ ( -- ptr u8 ) REC-N @ REC-RESERVE 0 REC-STORAGE BYTE-VIEW ;
: CODE-BUF@ ( -- ptr u8 ) CODE-LEN @ CODE-RESERVE 0 CODE-STORAGE BYTE-VIEW ;
: SITE-BUF@ ( -- ptr u8 ) SITE-N @ SITE-RESERVE 0 SITE-STORAGE BYTE-VIEW ;
: XT-BUF@ ( -- ptr u8 ) XT-N @ XT-RESERVE 0 XT-STORAGE BYTE-VIEW ;

: RESET ( -- ) 0 REC-N !  0 CODE-LEN !  0 SITE-N !  0 XT-N ! ;

\ Field f of record row r (shipped row 0, start 4, bytes 8, entry 12), of site
\ row s (code byte 0, kind 4, target 8) and of xt row x (cell row 0, shipped row 4).
: SH@ ( n n -- n )
   {: r:n f:n :}
   REC-BUF@ r REC-ROW * + f + AOT-BUF:W32@ ;
: SH-REC ( n -- n ) 0 SH@ ;
: SH-AT ( n -- n ) 4 SH@ ;
: SH-LEN ( n -- n ) 8 SH@ ;
: SH-ENTRY ( n -- n ) 12 SH@ ;
: SITE@ ( n n -- n )
   {: s:n f:n :}
   SITE-BUF@ s SITE-ROW * + f + AOT-BUF:W32@ ;
: XT@ ( n n -- n )
   {: x:n f:n :}
   XT-BUF@ x XT-ROW * + f + AOT-BUF:W32@ ;

private
variable LO
variable HI
public

\ The row whose emission holds shadow code byte n: the first row of the last
\ emission that starts at or below it. The rows' starts ascend because each
\ capture appends an emission after the last and rows its records at that start.
: ROW-AT ( n -- n )
   {: n:n :}
   0 LO !  REC-N @ HI !
   begin HI @ LO @ - 1 > while
      LO @ HI @ + 2 / {: mid:n :}
      mid SH-AT n <= if mid LO ! else mid HI ! then
   repeat
   begin LO @ 0 > if LO @ 1- SH-AT LO @ SH-AT = else false then while -1 LO +! repeat
   LO @ ;
;package

package AOT-BUF
public

\ boot-run name list: 0-terminated [len][name-bytes] records of the top-level entry
\ words (the REPL's INSTALL) the metabuild ran at the tail of the REPL
\ source.
$400 constant AOT-BOOTRUN-CAP
create AOT-BOOTRUN-BUF AOT-BOOTRUN-CAP allot    variable AOT-BOOTRUN-LEN

\ Protected WIDs owned by the captured runtime, window-relative.
\ One row can exist per protected WID below PROT-WID-MAX, so the bitmap's own
\ bound is also a fixed sufficient row bound.
PROT-WID-MAX constant AOT-PWIN-MAX
create AOT-PWIN-BUF AOT-PWIN-MAX 4 * allot    variable AOT-PWIN-N   \ packed u32

\ The checker payload holds signatures and the type-family registry.
\ THE ROW FORMAT IS THE CHECKER'S, NOT THIS FILE'S. A row is four u32 - name
\ offset, signature offset, package offset, visibility - and each offset names a
\ `[len u16][bytes]` record in the string section.
\ SIG-MAX IS THE RECORD BOUND: the capture emits at most one row per window
\ dictionary record.
16 constant SIG-ROW
AOT-REC-MAX constant AOT-SIG-MAX
DYNAMIC-BUFFER SIG-STORAGE n
variable AOT-SIG-N
\ The opaque pool carries verified effect graphs beside its names.
A64ICODE:AOT-SECTION-CAP constant AOT-SIG-STR-CAP
DYNAMIC-BUFFER SIG-STR-STORAGE n
variable AOT-SIG-STR-LEN

: AOT-SIG-STR-RESERVE ( n -- )
   dup 0< over AOT-SIG-STR-CAP > or if
      s" aot: effect pool exceeds the section byte budget" 74 die then
   CELL 1- + CELL / SIG-STR-STORAGE-RESERVE ;

: AOT-SIG-STR-BUF ( -- ptr u8 ) 0 SIG-STR-STORAGE BYTE-VIEW ;
\ The registry delta travels as OPAQUE BYTES with its own internal table, written
\ and read by the registry that owns those records.
$20000 constant AOT-REG-CAP
create AOT-REG-BUF AOT-REG-CAP allot    variable AOT-REG-LEN

: AOT-BLOB-BUF@ ( -- ptr u8 ) AOT-BLOB-BUF ;
: AOT-REC-BUF@ ( -- ptr u8 ) AOT-REC-BUF ;
: AOT-SITE-BUF@ ( -- ptr u8 ) AOT-SITE-BUF ;
: AOT-NAMES-BUF@ ( -- ptr u8 ) 0 AOT-NAMES-STORAGE BYTE-VIEW ;
: AOT-DSITE-BUF@ ( -- ptr u8 ) AOT-DSITE-BUF ;
: AOT-SIG-BUF@ ( -- ptr u8 )
   AOT-SIG-MAX SIG-ROW * CELL 1- + CELL / SIG-STORAGE-RESERVE
   0 SIG-STORAGE BYTE-VIEW ;
: AOT-SIG-STR-BUF@ ( -- ptr u8 ) AOT-SIG-STR-BUF ;
: AOT-REG-BUF@ ( -- ptr u8 ) AOT-REG-BUF ;

;package

package AOT-XTSITE
public
: BUF@ ( -- ptr u8 ) BUF ;
;package
package AOT-WINDOW
public
: BM-BUF@ ( -- ptr u8 ) BM-BUF ;
: VAL-BUF@ ( -- ptr u8 ) VAL-BUF ;
: XTOFF-BUF@ ( -- ptr u8 ) XTOFF-BUF ;
\ Field f of address-cell row c: its location u32 at 0, its target u32 at 4.
: XTOFF@ ( n n -- n )
   {: c:n f:n :}
   XTOFF-BUF@ c XTOFF-ROW * + f + AOT-BUF:W32@ ;
;package

package AOT-BUF
public

: AOT-BOOTRUN-BUF@ ( -- ptr u8 ) AOT-BOOTRUN-BUF ;
: AOT-PWIN-BUF@ ( -- ptr u8 ) AOT-PWIN-BUF ;

: CREC ( n -- ptr u8 )
   {: k:n :}
   AOT-REC-BUF@ AOT-REC-MAX 48 * + k AOT-CREC-ROW * + ;
: CREC@ ( n n -- n )
   {: k:n f:n :}
   k CREC f + W32@ ;
: PKG? ( n -- bool ) 16 CREC@ PKG-MARK = ;
\ A pool entry: its length byte, then its name.
: POOL$ ( n -- ptr u8 n )
   {: off:n :}
   AOT-NAMES-BUF@ off + {: e:ptr :}
   e 1+ e c@ ;
: CREC-NAME$ ( n -- ptr u8 n ) 8 CREC@ POOL$ ;
\ Protected-wid row r, a zero-based window offset.
: PWIN@ ( n -- n )
   {: r:n :}
   AOT-PWIN-BUF@ r 4 * + W32@ ;

;package

package AOT-SECTION
using AOT-BUF
public

: ROOM? ( n n -- bool ) {: used:n bytes:n :}
   used 0 < used A64ICODE:AOT-SECTION-CAP > or bytes 0 < or if 0 0 <> exit then
   bytes A64ICODE:AOT-SECTION-CAP used - <= ;

: REFUSE ( -- ) s" aot: encoded sections exceed their byte budget" A64ICODE:ICODE-EXIT-RC die ;

: +RAW ( n n -- n )
   2dup ROOM? 0= if REFUSE then + ;

: +BYTES ( n n -- n ) {: used:n bytes:n :}
   used bytes +RAW {: end:n :}
   bytes negate 3 and {: pad:n :}
   end pad ROOM? 0= if REFUSE then end pad + ;

: ROW-BYTES ( n n -- n ) {: count:n width:n :}
   count 0 < width 0 <= or if REFUSE then
   count A64ICODE:AOT-SECTION-CAP width / > if REFUSE then
   count width * ;

: +ROWS ( n n n -- n ) ROW-BYTES +BYTES ;

\ Seventeen scalar/count cells frame the common baked section.
\ THE BITMAP IS CHARGED IN ITS IMAGE FORM, which is the grouped one, so this
\ rebuilds it from the flat capture first: a merge can still have extended that
\ bitmap after the capture.
: BODY-SIZED ( n n n n -- n ) {: recbytes:n sitebytes:n databytes:n frames:n :}
   AOT-WINDOW:BM-BUF@ AOT-WINDOW:BM-LEN @ AOT-WINDOW:BM-COMPACT
   frames cells
   AOT-BLOB-LEN @ +BYTES
   recbytes +BYTES
   sitebytes +BYTES
   AOT-NAMES-LEN @ +BYTES
   databytes +BYTES
   AOT-WINDOW:XTOFF-N @ AOT-WINDOW:XTOFF-ROW +ROWS
   AOT-WINDOW:CBM-LEN +BYTES
   AOT-WINDOW:VAL-LEN @ +BYTES
   AOT-CSITE-N @ 4 +ROWS
   AOT-XTSITE:N @ 8 +ROWS
   AOT-SPAN:N @ AOT-SPAN:ROW +ROWS
   AOT-BOOTRUN-LEN @ 1+ +BYTES
   AOT-PWIN-N @ 4 +ROWS ;

: BODY-BYTES ( n -- n ) {: sitewidth:n :}
   AOT-REC-N @ AOT-CREC-ROW ROW-BYTES AOT-SITE-N @ sitewidth ROW-BYTES
   AOT-DSITE-N @ 4 ROW-BYTES 17 BODY-SIZED ;

\ The sidecar is emitted as one byte run: its sections have no internal pad.
: PAYLOAD-BYTES ( -- n )
   AOT-SIG-N @ AOT-SIG-STR-LEN @ or AOT-REG-LEN @ or 0= if 0 exit then
   56 AOT-SIG-N @ SIG-ROW +ROWS
   AOT-SIG-STR-LEN @ +RAW AOT-REG-LEN @ +RAW ;

: BYTES ( n -- n )
   BODY-BYTES
   8 +BYTES PAYLOAD-BYTES +BYTES ;

;package

\ The engine's `.` ends its line, and FMT:.INT is not in the engine: requiring
\ it here would load fmt below every capture window a test opens after this
\ file.
\ So the AOT layer carries its own: signed decimal, no space, no newline.
\ The digits come from the value made nonpositive, which keeps MIN-N in range.
package AOT-BUF
private
: NONPOS-DIGITS ( n -- )
   dup -10 > if negate [char] 0 + emit exit then
   dup 10 / RECURSE  10 mod negate [char] 0 + emit ;
public
: .INT ( n -- )
   dup 0< if [char] - emit else negate then NONPOS-DIGITS ;
;package
