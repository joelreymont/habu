\ aot-decl.f — the AOT capture format's storage: every buffer the capture fills,
\ every cap that bounds it, and the code-window budget derived from those caps.
\
\ WHY THIS IS ITS OWN FILE. Two processes fill these buffers, not one. The
\ metabuild host fills them from its own compiled words (src/habu/aot-capture.f)
\ and src/habu/habu2.f EMIT-AOT-SEED bakes them into the engine it writes; a
\ capture running INSIDE bin/hb fills the same buffers from the dictionary of the
\ engine that is booting, because the host's dictionary is not the target's and a
\ host-captured name does not exist in bin/hb (measured: three ordered deaths, an
\ ARM64-W32 duplicate, an ENGINE-ERROR duplicate, then a constant that would have
\ baked wrong data in silence). One declaration file is what lets those two
\ processes agree on a format instead of each carrying a copy of it.
\
\ Individual format caps and a shared aggregate byte budget live together.
\ AOT-SECTION checks actual encoded lengths, including framing and alignment,
\ before the engine emitter publishes any of this section.
\
\ WHO LOADS IT. It is a common engine-prefix source, immediately before habu2.f
\ in both builders (tools/bootstrap.sh SRC_COMMON, tools/build-fixpoint.f
\ BF-APPEND-COMMON), and it loads into a booted bin/hb behind
\ src/arch/arm64/asm.f, icode.f and
\ src/habu/layout.f, which is what AGREE and the caps need there.
\
\ THE PUBLIC TAILS KEEP THEIR `AOT-` PREFIX, and that is recorded debt rather
\ than a pattern: docs/forth.md § Packages calls a prefix-style public surface
\ legacy debt and asks for `AOT-BUF:BLOB-CAP`. The spellings are kept here so the
\ extraction moves no call site — the consumers import with `using AOT-BUF` and
\ read the same bare names they always read. Cleaning the tails is a separate,
\ measured job: 98 references in aot-capture.f, 89 in habu2.f and 29 in
\ test/aot-wid-build.f (20 of those inside GENERATED program text, which no
\ source-level rename sees), plus a per-tail collision check against the global
\ wordlist — layout.f already publishes a global MAX.
require src/habu/address-carrier.f
require src/habu/code-span.f
require src/habu/cell-grid.f

package AOT-BUF
public

\ A capture must hold any code window admitted by the fixed runtime region.
\ The fully guarded core measured 7,159,360 bytes; the old 3 MiB image limit
\ rejected it after compilation. Reuse the code budget, keeping capture and
\ file/merge bounds checks and the independent aggregate section budget.
CODE-BAND:BYTES constant AOT-BLOB-CAP
\ Capture storage belongs to the build, not the target DATA heap. A host and a
\ later source-bound writer can both be live without consuming two heap-sized
\ sets of buffers. The existing transient registry owns these mappings.
DYNAMIC-BUFFER BLOB-STORAGE n
: AOT-BLOB-BUF ( -- ptr u8 )
   AOT-BLOB-CAP CELL / BLOB-STORAGE-RESERVE
   0 BLOB-STORAGE BYTE-VIEW ;
variable AOT-BLOB-LEN
\ A capture window is a dictionary subrange, so DICT-CAP - 65536 records - is
\ the absolute ceiling one can reach and any bound under it refuses a window the
\ dictionary itself would hold. The line here read 16384 "because the chain
\ needs ~6554"; the chain had grown to 16382 records by the time the next
\ twenty definitions in src/ hit the bound. This is that bound doubled, measured
\ at 16402 records for the build that first exceeded it. A build that crosses it
\ prints the captured count, this bound and the name of the record being added
\ before it dies `aot-capture: too many records` (ACAP-REC-REFUSE in
\ src/habu/aot-capture.f); a record carries no file, so that name is the whole
\ locator the refusal has. test/aot-capture-bound.f pins those lines over a
\ private copy of this file with the bound lowered; because AOT-SIG-MAX below is
\ this same constant, a lowered bound refuses a window of checked words at the
\ signature buffer first, and that fixture's window is packages.
32768 constant AOT-REC-MAX
\ AOT-REC-BUF holds three regions (all viewed via AOT-REC-BUF@, no extra TRUST):
\   [0 .. MAX*48)              verbatim 48B dict records (capture source of truth)
\   [MAX*48 .. +MAX*CREC-ROW)  compact 20B records (three u32 role/code/name fields + metadata + wid)
\   [+MAX*CREC-ROW .. +48)     48B scratch for the build-time expand==verbatim proof
DYNAMIC-BUFFER REC-STORAGE n
: AOT-REC-BUF ( -- ptr u8 )
   AOT-REC-MAX 48 * AOT-REC-MAX AOT-CREC-ROW * + 48 + CELL / REC-STORAGE-RESERVE
   0 REC-STORAGE BYTE-VIEW ;
variable AOT-REC-N
\ A call-site row: three u32 words - blob-off, name-off, and the callee's SCOPE,
\ the wordlist the seed resolves the name in. A wid below FIRST-DYNAMIC-WID is a
\ layout constant and passes through, a window coordinate is rebased like a
\ record's, and WID-QUAL says the pooled name is qualified and carries its own
\ scope (a pre-window package's word, whose id is that engine's own number).
\ Named without the AOT- prefix the older tails here carry: that prefix is the
\ recorded debt this file's header names, and a new name does not join it.
12 constant SITE-ROW
\ Only the baked image may bind primitive BLs to a canonical text distance.
\ Bit 32 selects bound sites; bit 33 independently selects packed boot rows.
\ Reusable artifacts always retain SITE-ROW named rows and untagged counts.
$100000000 constant SITE-BOUND-TAG
$200000000 constant SEED-PACK-TAG
$FFFFFFFF constant SITE-COUNT-MASK
4 constant BOUND-SITE-ROW
\ The IMAGE's row is the same three words, but its middle word is a TARGET
\ rather than a name: the build resolves the callee against the two tables the
\ image carries - the seeded primitives and the payload's own records - and
\ stores its index in the dictionary the boot builds, so the seed relocates the
\ site with a load instead of a lookup (habu2.f EMIT-AOT-SITES binds,
\ EM-AOT-PATCH-SITES relocates).
\ A CALLEE THE IMAGE DOES NOT CARRY KEEPS ITS NAME, and that is not an edge
\ case: a partial capture - a stripped application, a chain capture, any window
\ smaller than the whole tree - calls words the ENGINE it boots into has from
\ its own prefix, and no index of this payload can name one. Such a row sets
\ SITE-NAME-TAG on the target and carries the pool offset in the low bits; the
\ scope word is then the wordlist the seed resolves the name in, exactly as
\ before. Both generic kinds travel in one table. A complete runtime whose
\ every site is a primitive BL can instead carry SITE-BOUND-TAG and four-byte
\ offsets: its emitted BLs already hold their canonical target displacement.
\ THE TWO INDEX KINDS ARE NOT THE SAME NUMBER. A primitive's index is absolute:
\ the seed copies LDICT to dict[0..LNCOUNT) in every image. A payload record's
\ is RELATIVE to wherever the records pass put them, and that is not LNCOUNT in
\ a partial image: a stripped application or a chain capture compiles its own
\ cold prefix into the dictionary before the payload registers. So the row says
\ which kind it carries and the seed derives the payload base from the table it
\ just wrote (NDICT - LAOTNREC), never from a build-time count.
$80000000 constant SITE-NAME-TAG
$40000000 constant SITE-REC-TAG
$3FFFFFFF constant SITE-TARGET-MASK
$FFFFFFFE constant WID-QUAL

\ The guarded core captures 124,137 call rows. Add 25% headroom and round up
\ to 32,768-row units: 163,840 rows, a fixed 1,966,080-byte table. Guards add
\ calls at entries and transfers; the old 32,768-row budget no longer fits.
$28000 constant AOT-SITE-MAX
create AOT-SITE-BUF AOT-SITE-MAX SITE-ROW * allot    variable AOT-SITE-N   \ packed rows: blob-off u32 + name-off u32 + callee scope u32
\ THE ONE DYNAMIC BUFFER THE CAPTURE'S WALK NEVER SEES, AND IT IS STRUCTURE THAT
\ EXEMPTS IT RATHER THAN A LIST. The pool grows to its used size because its cap is
\ DICT-CAP * 256 (16 MiB) against a measured ~51 KiB need, so a static buffer is not
\ an option. It is also alive long after the capture: habu2.f EMIT-AOT-SEED bakes
\ the section by reading AOT-NAMES-BUF@ during the image emit, and aot-file.f MERGE
\ reads and writes it too, both after AOT-CAPTURE:CAPTURE has returned - so giving
\ its mapping back inside the capture would emit an empty name pool and every
\ seeded engine would lose every name.
\
\ Nothing has to remember that. This file is build-host source: tools/native-build.f
\ requires it before AOT-ARM:WINDOW-OPEN, and tools/hb-build-lib.f and
\ bootstrap/cg/forth.fs carry it in the stage prefix, so its control record is
\ always BELOW the captured DATA window and is never baked. The record therefore
\ registers in the HOST's DYNAMIC-STORAGE instance, and the capture walks the
\ window's instance only (src/habu/aot-capture.f ACAP-RELEASE-DYNAMIC, span-checked
\ against the captured code band). Moving this declaration above a window open
\ would put it in a window's registry and the walk would then take it away
\ mid-capture, which is what this note is here to prevent.
DYNAMIC-BUFFER AOT-NAMES-STORAGE n
variable AOT-NAMES-LEN
: AOT-NAMES-RESERVE ( n -- ) CELL 1- + CELL / AOT-NAMES-STORAGE-RESERVE ;
\ DATA relocation rows contain a blob offset. Bit 31 selects a raw address
\ cell in a recognised metadata trailer; otherwise it names an address chain.
\ Both move by (seed-DP - AOT-DATA-D0), preserving their window coordinate.
$80000000 constant AOT-DSITE-CELL
$7FFFFFFF constant AOT-DSITE-OFF-MASK
\ DATA and CODE rows partition the aligned relocation starts in the blob.
\ At most one row can name each four-byte word, including deduplicated defer
\ metadata cells. This is a format bound; allocate only the rows actually used.
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
\ the same thing in two processes, so the seed maps an in-window wid to
\ T0 + (wid - W0) and refuses every other non-zero wid by name.
\
\ THE ROWS STORE THE OFFSET, NOT THE ID, and these two cells are the capturing
\ process's own and do not travel. W0 is whatever the BUILDING engine had already
\ allocated when the window opened, so a row that stored an id would carry that
\ engine's history: measured, an engine built from a tree with one package more
\ than its host wrote 3305 different wid bytes for the same tree. A row therefore
\ holds `wid - W0 + WID-REL-BASE`, wid 0 holds 0, and the payload's own base cell
\ (habu2.f AOT-WINDOW:LWIDW0) is that constant. One-based, because the window's
\ FIRST wordlist is one records name and a zero-based offset would spell it the
\ way the global wordlist is spelled. The protected-wordlist rows are offsets
\ too, and zero-based: that table lists ids and has no marker to collide with.
1 constant WID-REL-BASE
variable AOT-WID-W0    variable AOT-WID-SPAN
\ The window's DATA SPAN, and the offsets the seed must not take from its content.
\ Reserving the span zeroed was right only while every byte in it was zero. It is
\ not: the REPL sources put real bytes there - a TRUST row's name and signature
\ are `s"` literals interned into DP - and those arrived in the seeded engine as
\ zeros. So the content travels, but as RUNS rather than as the span (AOT-WINDOW
\ below); AOT-DATA-SIZE is the span itself and, with a sparse payload, the only
\ authority for it.
\ A DECLARED ADDRESS CELL IS THE ONE THING THE CONTENT MAY NOT CARRY RAW. An XT
\ cell holds a code address in the BUILDING host; a DATA-pointer cell can hold an
\ address in that host's captured window. Neither address is valid after seeding.
\ Both cells are therefore excluded from every run and listed with their kind.
\ The seed reconstructs nulls and window-relative targets; an exact prefix CODE
\ entry travels under its resolving global or public name. The first u32 locates
\ the cell, and the second carries the target kind and offset or name-pool entry.

;package

package AOT-WINDOW
public
\ THE WINDOW'S CONTENT TRAVELS AS CELLS, AND THE SPAN IS A NUMBER.
\ The window is a dictionary subrange, so almost all of it is `allot`ed room that
\ nothing has written: the compiler chain's window measures 1,531,045 bytes of
\ span and 32 bytes of content, in four cells, 99.998% zero (dot
\ habu-census-the-captured-fe5f7c49). Storing the span verbatim baked 1.5 MB of
\ zeros into every engine. So what is stored is a PRESENCE BITMAP over the
\ window's cells and one varint per non-zero cell, and the seed zeroes the span
\ and lays those cells into it.
\
\ THE SPAN IS NO LONGER A LENGTH ANYWHERE, which is why AOT-DATA-SIZE is now a
\ genuine scalar in the artifact rather than a section's length. SPAN-CAP bounds
\ it, and it bounds nothing else: the window costs no buffer here now, so the cap
\ exists only to keep the u32 offsets in the two tables below honest and to fail
\ closed on a span no engine could reserve. 16 MiB against the full runtime's
\ measured 8,310,765 bytes leaves honest headroom at no storage cost.
$1000000 constant SPAN-CAP
\ THE ENCODING IS src/habu/cell-grid.f's, shared with the snapshot writer: a
\ presence bitmap over the window's cells, grouped, and one varint per present
\ cell. These names are its constants under the window's own package, where
\ the capture, both emitted readers and the tools read them.
CELL-GRID:CELL-BYTES constant CELL-BYTES
CELL-GRID:CELL-BITS constant CELL-BITS
CELL-GRID:BM-BYTE-SPAN constant BM-BYTE-SPAN
CELL-GRID:GROUP-BYTES constant GROUP-BYTES
CELL-GRID:GROUP-SPAN constant GROUP-SPAN
CELL-GRID:VMAX constant VMAX
\ THE GRID IS THE DATA CELL GRID, NOT THE WINDOW'S OWN. Every declared address
\ cell in the window is eight-byte aligned in DATA - the atomics fault on a
\ misaligned cell - and each must fall on exactly one grid cell, because the
\ capture leaves its bits clear and lets the seed write the value. So a window
\ whose base is not cell-aligned is refused by its writer, and a span is rounded
\ UP to a whole number of cells: the bytes it gains are above the captured DP,
\ never read, and zero by construction. One grid also makes a merge a splice:
\ two windows captured against cell-aligned bases share it, so an appended
\ artifact's bitmap and values concatenate once its base is padded to a bitmap
\ byte (src/habu/aot-file.f PLACE-WDATA).
\ The bitmap's byte ceiling is the span cap's own arithmetic, so a window this
\ refuses is one no engine could reserve. Measured: 130,417 bytes for the
\ release engine's 8,346,672-byte window.
SPAN-CAP BM-BYTE-SPAN / constant BM-CAP
DYNAMIC-BUFFER BM-STORAGE n
: BM-BUF ( -- ptr u8 )
   BM-CAP CELL / BM-STORAGE-RESERVE
   0 BM-STORAGE BYTE-VIEW ;
variable CELL-N                      \ present cells, which the encoded bytes no longer state
variable BM-LEN                      \ bytes of bitmap, trailing absent cells dropped
variable CONTENT-END                 \ one past the last present cell: where a merge may pad to

\ ---- the image's second level: a presence bit per GROUP-BYTES bitmap bytes -----
\ THE GROUPING IS AN IMAGE ENCODING, NOT THE CAPTURE FORMAT. The capture, the
\ artifact sections and a merge keep the flat bitmap, where appending a window is
\ a concatenation (src/habu/aot-file.f PLACE-WDATA) and a cell's bit is at a
\ fixed place; BM-COMPACT groups that bitmap once for a writer, and both emitted
\ readers (src/habu/habu2.f AOT-WINDOW:APPLY-CELLS for the baked window,
\ src/habu/aot-lib.f EMIT-DATA-COPY for a stripped image) decode it with one
\ outer loop over the presence bits around the same per-byte walk.
\ Image form, in both writers: [groups G][stored bytes S] and then the
\ src/habu/cell-grid.f form: presence map, present groups, values.
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

\ The one run the image carries for the bitmap: the presence map and the groups
\ it says are there. Derived, never stored, so it cannot disagree with them.
: CBM-LEN ( -- n ) CBM-PMAP-BYTES CBM-STORED @ + ;

\ A flat bitmap in, the compact form in CBM-BUF. It is a function of the bitmap
\ it is handed and holds nothing between calls, which is why a merge that
\ extends the bitmap needs no invalidation here - the next accounting rebuilds
\ it (AOT-SECTION:BODY-BYTES). The group cap keeps both halves inside CBM-CAP.
: BM-COMPACT ( ptr u8 n -- ) {: bm:ptr len:n :}
   len CELL-GRID:GROUPS GROUP-CAP > if
      s" aot: the window bitmap exceeds the AOT group map" 74 die then
   bm len CBM-BUF CELL-GRID:COMPACT CBM-STORED ! CBM-GROUPS ! ;

\ A present cell costs its own varint and no more, so this cap answers to the
\ content: the release engine's window encodes its 300,575 present cells in
\ 834,153 value bytes, 2.8 bytes a cell, and the arithmetic worst case for that
\ many cells is VMAX each. $400000 leaves 5x over the measured figure and covers
\ a window of 419,430 pointer-valued cells, which is 3.3 MB of live cells.
\ Overflow is refused by name.
$400000 constant VAL-CAP
DYNAMIC-BUFFER VAL-STORAGE n
: VAL-BUF ( -- ptr u8 )
   VAL-CAP CELL / VAL-STORAGE-RESERVE
   0 VAL-STORAGE BYTE-VIEW ;
variable VAL-LEN

\ Every producer of a window table starts from the same four numbers, so no
\ caller can zero three of them and leave the fourth describing the last capture.
: WINDOW-RESET ( -- )
   0 CELL-N !  0 BM-LEN !  0 CONTENT-END !  0 VAL-LEN ! ;

\ ---- the value codec -----------------------------------------------------------
\ src/habu/cell-grid.f owns the varint; these are its words under the window's
\ package, where the capture, the packer and the linker call them.
: CELL-VLEN ( n -- n ) CELL-GRID:CELL-VLEN ;

: CELL-V! ( n ptr u8 -- n ) CELL-GRID:CELL-V! ;

: CELL-V@ ( ptr u8 n -- n n ) CELL-GRID:CELL-V@ ;

\ Address rows grow independently of the engine's declaration registry. The
\ artifact's aggregate byte budget, checked before copy or emission, is the
\ limit; this single-section ceiling also bounds each allocation request.
8 constant XTOFF-ROW
AOT-SECTION-CAP XTOFF-ROW / constant XTOFF-MAX
$80000000 constant XTOFF-WINDOW-TAG
$80000000 constant XTOFF-DATA-TAG
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
\ two high bits select CODE (00), named CODE (01), or DATA (10); 11 is invalid.
\ CODE/DATA low bits are zero for null or the target-window offset plus one.
\ Named CODE carries a nonzero name-pool entry offset plus one.
;package

package AOT-BUF
public

\ CODE-literal relocation table (fourth relocation class): blob offsets of the
\ movz/movk x9 literals whose value lands in the captured code range [B0,B1) --
\ anonymous quotation-body entry addresses (J-SEMIQUOT `C-CODE-ADDR QENT`). Rebased by
\ the code delta (seedCP - captureB0); no name (quotations are anonymous). Stored in
\ the DATA-site buffer right after the AOT-DSITE-N DATA offsets (one fewer scratch
\ view), and baked as its own contiguous LAOTCSITES section.
variable AOT-CSITE-N
variable AOT-CODE-B0
\ NAMED code sites (fifth relocation class): blob offsets of movz/movk literals
\ whose value is the ENTRY OF A WORD, paired with that word's name in the pool.
\ The seed LFINDs the name in the engine it is booting and writes the xt into the
\ four immediate lanes -- the same answer the call-site table gets for a BL, for a
\ site that is not a BL. A capture stores 0 in the lanes, so the baked blob carries
\ no host address and the patch is the only thing that can put a real one there.
\ ITS PRODUCER IS ACAP-OUT-CHAIN (aot-capture.f), which classifies every recorded
\ chain the window's DATA span does not hold: in-window code goes to the CODE sweep,
\ a value ACAP-TGT>REC resolves to a record's entry becomes a row here, and anything
\ else ends the build. The class the row exists for is a code literal naming a
\ PRE-WINDOW word (`['] X`), which no delta relates to the target and
\ the inliner decline cannot reach; ACAP-OUT-CHAIN carries that argument. The format
\ is baked into the engine, so it migrates once - a row kind added later is a second
\ migration of every baked-code route. In-window code literals stay b0-relative:
\ rebasing them is correct and costs no lookup.

;package

package AOT-XTSITE
public
16384 constant MAX
create BUF MAX 8 * allot    variable N   \ 8B rows: blob-off u32 + name-off u32
;package

\ THE CODE SPANS OF THE WORDS THE IMAGE SHIPS NO RECORD FOR. A record is what
\ makes a word reachable by name, and a private word nothing can qualify into is
\ reachable by nothing - so its record buys no caller anything and does not
\ travel (aot-capture.f ACAP-NAMED? decides). Its CODE does travel, called by a
\ displacement the blob carries, and one reader still has to account for it:
\ src/habu/aot-closure.f walks the code a `hb-build` is shaking out and retargets
\ every PC-relative branch against the span that owns it. A row here is that
\ account, and it is the whole of it - the two u32 a record row would have
\ carried, and none of the eighteen bytes of name, flags, kind and wordlist that
\ only a lookup needs. 20 bytes a word becomes 8, and 48 bytes of booted
\ dictionary become none.
\ A ROW IS (blob offset u32, raw code span u32), in capture order, the same two
\ fields and the same CODE-SPAN encoding a compact record's first two words hold,
\ so the build proves a span row against the record it replaced field for field.
\ The seed publishes the table's address, this count and the blob's landing base
\ in the three AOT-SPAN cells layout.f set aside.
package AOT-SPAN
private
\ RESERVED AT FIRST USE, NOT ALLOTTED, and that is the shape every table only
\ the capture fills now has (REC-STORAGE above is the original). A static
\ `create BUF MAX ROW * allot` spends MAX * ROW = 262,144 bytes of the build
\ host's DATA prefix in every process that loads this file, and the prefix is
\ the DATA below the captured window (AOT-ARM:WINDOW-OPEN takes D0 from `here`),
\ so those bytes are charged to every build whether or not it captures anything
\ and they buy the engine nothing. A mapping costs the prefix nothing and the
\ bound is unchanged, so a lowered AOT-REC-MAX still refuses where it did.
DYNAMIC-BUFFER STORAGE n
public
AOT-BUF:AOT-REC-MAX constant MAX      \ at most one row per window record
8 constant ROW
variable N
: BUF@ ( -- ptr u8 )
   MAX ROW * CELL 1- + CELL / STORAGE-RESERVE
   0 STORAGE BYTE-VIEW ;
;package

\ THE SHADOW: A SECOND TARGET'S ROUTINES FOR THE WINDOW'S RECORDS. A cross-build
\ compiles every tier-1 definition twice, the host's routine into the live region
\ and the target's into src/compiler/native/shadow.f's map (docs/x86-64.md "Dual
\ emission"), and the target's image is written from the capture, so the capture
\ carries that map. src/habu/aot-shadow.f fills these tables; a capture taken with
\ no shadow open leaves all four empty, and the ARM64 seed reads none of them.
\
\ FOUR TABLES, AND NO HOST ADDRESS IN ANY OF THEM. CODE is every emission's bytes,
\ each emission once, as its own unplaced routine: a call's field is the zero an
\ unplaced emission writes, a DATA literal holds its window DATA offset, a code
\ literal naming a word holds 0, and a `codeaddr` holds its function's offset in
\ its own emission. A RECORD row is (window record index, emission start in CODE,
\ emission bytes, entry offset in it): a `does>` definer and its companion are two
\ rows over one emission, the companion entering where the clause function
\ starts. A SITE row is (byte offset in CODE, kind, target); a target is a window
\ record, SITE-REC-TAG beside its window index, or a word of the engine's own
\ prefix, SITE-NAME-TAG beside its name's pool offset (a qualified name when its
\ package is not global), the two forms AOT-BUF's image site rows use. An XT row
\ is (address-cell row, window record index): an address cell whose CODE target
\ is a window record's entry, keyed by the record and not by the ARM64 blob
\ offset the address-cell row itself carries.
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
\ An emission is code the window's own region held, so the blob's bound is its
\ bound; one record row per window record; a site is a call or a ten-byte MOVABS,
\ so a byte can start at most one in five; one XT row per address-cell row.
AOT-BUF:AOT-BLOB-CAP constant CODE-CAP
AOT-BUF:AOT-REC-MAX constant REC-MAX
CODE-CAP 5 / constant SITE-MAX
AOT-WINDOW:XTOFF-MAX constant XT-MAX

\ The site kinds. CALL and TAIL hold a rel32 to their target, CODE a MOVABS of
\ the target's entry, DATA a MOVABS of a window DATA offset, FUN a MOVABS of a
\ function's offset in the site's own emission.
1 constant CALL
2 constant TAIL
3 constant DATA
4 constant CODE
5 constant FUN

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
;package

package AOT-BUF
public

\ boot-run name list: 0-terminated [len][name-bytes] records of the top-level entry
\ words (the REPL's INSTALL) the metabuild ran at the tail of the REPL
\ source. With the source dropped, EM-SEED-AOT LFINDs + calls each after RX/flush so
\ the seeded engine installs the REPL with no embedded source.
\ THIS LIST RUNS ON EVERY BOOT, and it used to run on one. The seed fires at the
\ end of the engine prefix stream (EM-COMPILE-EXIT, LEX0) whatever the mode is, so
\ a piped program, a `--load` tool run and a tty REPL all reach their first user
\ token with the blob copied, the records registered and this list walked. The
\ entry words self-guard - INSTALL asks TTY? before installing a REPL - so a batch
\ boot runs them and gets no REPL, which is a different thing from not running
\ them. The old contract was the opposite (armed at the interactive REPL entry and
\ nowhere else, dot habu-decide-arm-the-5234727b), which is why anything written
\ before 2026-08-14 that says a captured word is missing from a batch dictionary,
\ or that observing the seed needs a tty, is describing the retired shape.
$400 constant AOT-BOOTRUN-CAP
create AOT-BOOTRUN-BUF AOT-BOOTRUN-CAP allot    variable AOT-BOOTRUN-LEN

\ Protected WIDs owned by the captured runtime, window-relative.  The cold
\ baseline starts with an empty bitmap; the seed rebases these rows after it has
\ registered the window's wordlists.  No build-host WID survives the cut.
\ One row can exist per protected WID below PROT-WID-MAX, so the bitmap's own
\ bound is also a fixed sufficient row bound.
PROT-WID-MAX constant AOT-PWIN-MAX
create AOT-PWIN-BUF AOT-PWIN-MAX 4 * allot    variable AOT-PWIN-N   \ packed u32

\ ---- the checker payload: signatures and the type-family registry -------------
\ WHY AN AOT ENGINE NEEDS THEM. A seed puts a word in the RUNTIME dictionary and
\ nothing in the checker's record set, so a `:` definition naming a seeded word
\ dies E-UNDEFINED at that token even though the engine can call it. These three
\ buffers carry what closes that: one row per checked window word, the strings
\ those rows name, and the type-family registry delta the signatures resolve
\ against.
\
\ THE ROW FORMAT IS THE CHECKER'S, NOT THIS FILE'S. A row is four u32 - name
\ offset, signature offset, package offset, visibility - and each offset names a
\ `[len u16][bytes]` record in the string section, which is src/core/checker.f's
\ signature-pool arena copied verbatim. The artifact is a courier for that arena:
\ the only thing a merge does to a row is move its three offsets by the length of
\ the pool it is appended behind.
\ SIG-MAX IS THE RECORD BOUND, not a measurement: the capture emits at most one
\ row per window dictionary record, so a window that fits AOT-REC-MAX records
\ cannot produce more rows than that.
16 constant SIG-ROW
AOT-REC-MAX constant AOT-SIG-MAX
\ AOT-SIG-MAX * SIG-ROW is 524,288 bytes, the largest of the three tables that
\ only the capture fills, so it is reserved at first use for the reason
\ AOT-SPAN:BUF@ above carries: a static allot spends those bytes of the build
\ host's DATA prefix in every process that loads this file.
DYNAMIC-BUFFER SIG-STORAGE n
variable AOT-SIG-N
\ The opaque pool carries verified effect graphs beside its names. Its format
\ bound is the section budget; allocate only the admitted bytes, like names
\ and relocation rows, rather than imposing the former text-only size guess.
AOT-SECTION-CAP constant AOT-SIG-STR-CAP
DYNAMIC-BUFFER SIG-STR-STORAGE n
variable AOT-SIG-STR-LEN

: AOT-SIG-STR-RESERVE ( n -- )
   dup 0< over AOT-SIG-STR-CAP > or if
      s" aot: effect pool exceeds the section byte budget" 74 die then
   CELL 1- + CELL / SIG-STR-STORAGE-RESERVE ;

: AOT-SIG-STR-BUF ( -- ptr u8 ) 0 SIG-STR-STORAGE BYTE-VIEW ;
\ The registry delta travels as OPAQUE BYTES with its own internal table, written
\ and read by the registry that owns those records (src/core/type-family.f and
\ src/core/type-schema.f through the checker's REG-EXT hook). Record widths and
\ store count live with the stores; this file carries the bytes and the cap.
\ $20000 against the chain's measured 45,666: 70 families, 317 sum variants, 43
\ product fields, 43 schema nodes and 3,778 bytes of interned type-name string.
$20000 constant AOT-REG-CAP
create AOT-REG-BUF AOT-REG-CAP allot    variable AOT-REG-LEN

\ Expose the build-scratch buffers for the checked copy/BYTES, sites below.
\ The blob, record, site, name, relocation, and boot-run accessors refine their
\ respective scratch buffers.
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
;package

package AOT-BUF
public

: AOT-BOOTRUN-BUF@ ( -- ptr u8 ) AOT-BOOTRUN-BUF ;
: AOT-PWIN-BUF@ ( -- ptr u8 ) AOT-PWIN-BUF ;

;package


\ The shared physical budget is charged for actual encoded lengths. Individual
\ tables retain their own format limits; growing one no longer reserves every
\ other table's maximum in the aggregate.
\ Final-image streams are transient build products. Fixed capture rows remain
\ the reusable authority; sizing and emission consume these same built bytes.
package AOT-PACK
using AOT-BUF

DYNAMIC-BUFFER RECS n
DYNAMIC-BUFFER CALLS n
DYNAMIC-BUFFER DATA-SITES n
PTR-VARIABLE DST
variable CUR variable LIMIT variable PREV

: REFUSE ( -- ) s" aot: invalid packed seed metadata" 74 die ;
: U32? ( n -- ) dup 0< swap $FFFFFFFF > or if REFUSE then ;
: W32@ ( ptr u8 -- n ) {: p:ptr :}
   0 4 0 ?do p i + c@ i 8 * lshift or loop ;
: BEGIN-STREAM ( ptr u8 n -- ) LIMIT ! DST ! 0 CUR ! ;
: V+ ( n -- ) {: v:n :}
   v U32?
   v AOT-WINDOW:CELL-VLEN LIMIT @ CUR @ - > if REFUSE then
   v DST @ CUR @ + AOT-WINDOW:CELL-V! CUR +! ;
: OFF>Q ( n -- n )
   dup U32? dup 3 and 0<> if REFUSE then 4 / ;
\ The seed packs a span in four-byte units. An exact span may end on any byte,
\ so one that is not whole units is refused here rather than truncated.
: SPAN>Q ( n -- n ) {: raw:n :}
   raw CODE-SPAN:VALID? 0= if REFUSE then
   raw CODE-SPAN:BODY 3 and 0<> if REFUSE then
   raw CODE-SPAN:BODY 4 / 2 * raw CODE-SPAN:FULL? if 1+ then ;
: META>Q ( n -- n ) {: m:n :}
   m $FFFC00F0 and 0<> if REFUSE then
   m 15 and m 8 rshift $FF and 4 lshift or
   m 16 rshift 3 and 12 lshift or ;
: ZIG ( n -- n ) dup 0< if negate 2 * 1- else 2 * then ;
: COUNT-CHECK ( n n -- n ) {: count:n max:n :}
   count 0< count max > or if REFUSE then count ;

: REC+ ( ptr u8 -- ) {: rec:ptr :}
   rec 16 + W32@ {: wid:n :}
   wid $FFFFFFFF = if
      0 V+ rec W32@ V+ rec 4 + W32@ V+
   else
      wid 1+ V+ rec W32@ OFF>Q V+ rec 4 + W32@ SPAN>Q V+
   then
   rec 8 + W32@ V+ rec 12 + W32@ META>Q V+ ;

public
variable REC-LEN variable CALL-LEN variable DATA-LEN
: REC-BUF ( -- ptr u8 ) 0 RECS BYTE-VIEW ;
: CALL-BUF ( -- ptr u8 ) 0 CALLS BYTE-VIEW ;
: DATA-BUF ( -- ptr u8 ) 0 DATA-SITES BYTE-VIEW ;

: BUILD ( bool -- ) {: bound:bool :}
   AOT-REC-N @ AOT-REC-MAX COUNT-CHECK {: recn:n :}
   AOT-SITE-N @ AOT-SITE-MAX COUNT-CHECK {: calln:n :}
   AOT-DSITE-N @ AOT-DSITE-MAX COUNT-CHECK {: datan:n :}
   0 REC-LEN ! 0 CALL-LEN ! 0 DATA-LEN !
   recn 0 > if
      recn 25 * {: recbytes:n :}
      recbytes CELL 1- + CELL / RECS-RESERVE
      REC-BUF recbytes BEGIN-STREAM
      recn 0 ?do
         AOT-REC-BUF@ AOT-REC-MAX 48 * + i AOT-CREC-ROW * + REC+
      loop CUR @ REC-LEN !
   then
   bound calln 0 > and if
      calln 5 * {: callbytes:n :}
      callbytes CELL 1- + CELL / CALLS-RESERVE
      CALL-BUF callbytes BEGIN-STREAM 0 PREV !
      calln 0 ?do
         AOT-SITE-BUF@ i SITE-ROW * + W32@ OFF>Q {: off:n :}
         off PREV @ - ZIG V+ off PREV !
      loop CUR @ CALL-LEN !
   then
   datan 0 > if
      datan 5 * {: databytes:n :}
      databytes CELL 1- + CELL / DATA-SITES-RESERVE
      DATA-BUF databytes BEGIN-STREAM 0 PREV !
      datan 0 ?do
         AOT-DSITE-BUF@ i 4 * + W32@ {: row:n :}
         row AOT-DSITE-OFF-MASK and OFF>Q {: off:n :}
         off PREV @ - ZIG 2 * row AOT-DSITE-CELL and 0<> if 1+ then V+
         off PREV !
      loop CUR @ DATA-LEN !
   then ;

;package

package AOT-SIG
public
: INSTALL-NAME$ ( -- ptr u8 n ) s" CK-AOT-REG-INSTALL" ;
;package

package AOT-SECTION
using AOT-BUF
public

: ROOM? ( n n -- bool ) {: used:n bytes:n :}
   used 0 < used AOT-SECTION-CAP > or bytes 0 < or if 0 0 <> exit then
   bytes AOT-SECTION-CAP used - <= ;

: REFUSE ( -- ) s" aot: encoded sections exceed their byte budget" ICODE-EXIT-RC die ;

: +RAW ( n n -- n )
   2dup ROOM? 0= if REFUSE then + ;

: +BYTES ( n n -- n ) {: used:n bytes:n :}
   used bytes +RAW {: end:n :}
   bytes negate 3 and {: pad:n :}
   end pad ROOM? 0= if REFUSE then end pad + ;

: ROW-BYTES ( n n -- n ) {: count:n width:n :}
   count 0 < width 0 <= or if REFUSE then
   count AOT-SECTION-CAP width / > if REFUSE then
   count width * ;

: +ROWS ( n n n -- n ) ROW-BYTES +BYTES ;

\ Seventeen scalar/count cells frame the common baked section. BYTES, rounds
\ each byte run to four bytes; packed rows already have that alignment.
\ THE BITMAP IS CHARGED IN ITS IMAGE FORM, which is the grouped one, so this
\ rebuilds it from the flat capture first: a merge can still have extended that
\ bitmap after the capture, and src/habu/habu2.f EMIT-AOT-SEED takes this count
\ immediately before it emits those same bytes.
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

: SEED-BODY ( bool -- n ) {: bound:bool :}
   AOT-PACK:REC-LEN @
   bound if AOT-PACK:CALL-LEN @ else AOT-SITE-N @ SITE-ROW ROW-BYTES then
   AOT-PACK:DATA-LEN @ bound if 20 else 19 then BODY-SIZED ;

\ The sidecar is emitted as one byte run: its sections have no internal pad.
: PAYLOAD-BYTES ( -- n )
   AOT-SIG-N @ AOT-SIG-STR-LEN @ or AOT-REG-LEN @ or 0= if 0 exit then
   56 AOT-SIG-N @ SIG-ROW +ROWS
   AOT-SIG-STR-LEN @ +RAW AOT-REG-LEN @ +RAW ;

: BYTES ( bool n -- n ) {: sidecar:bool sitewidth:n :}
   sitewidth BODY-BYTES
   sidecar if
      8 +BYTES PAYLOAD-BYTES +BYTES
      AOT-SIG:INSTALL-NAME$ nip +BYTES
   then ;

: SEED-BYTES ( bool bool -- n ) {: sidecar:bool bound:bool :}
   bound SEED-BODY
   sidecar if
      8 +BYTES PAYLOAD-BYTES +BYTES
      AOT-SIG:INSTALL-NAME$ nip +BYTES
   then ;

;package
