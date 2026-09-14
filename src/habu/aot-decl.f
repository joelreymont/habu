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
\ ---- the address chain both sides have to recognise ------------------------
\ package SNAP-RELOC is opened by src/habu/layout.f (bands and exit statuses),
\ reopened by src/habu/habu2.f (labels and the relocation passes) and by
\ src/habu/snap-lib.f (the writer's half); this is its fourth file and it carries
\ the one thing a CAPTURE needs from that package - the shape of the chain, which
\ is what makes an address literal recognisable in a code blob. It sits with the
\ capture buffers because the two readers are the emitter's relocation pass and
\ the capture's own site scan, and a shape read two ways is a shape that drifts.
\ The reader and writer of a chain's VALUE live here too, for the same reason.
package SNAP-RELOC
public

\ The shape of the four-instruction address chain habu2.f C-ADDR-RAW emits, as the
\ relocation pass has to read it back. ADDR-OPC-MASK keeps everything in a MOVZ or
\ MOVK word except its 16-bit immediate, so masking a site word leaves the opcode,
\ the shift and the destination register to compare. ADDR-IMM-MASK is that
\ immediate once it has been shifted down by five, and ADDR-CHAIN-BYTES is the
\ whole chain.
\ W-MOVZ0/W-MOVK1/W-MOVK2/W-MOVK3 all name x9, because that is the register the ONE
\ carrier C-ADDR-RAW writes into, but they are not the only chains that reach the
\ map: a chain the native compiler emits names whichever register its allocator
\ chose. So the pass takes the register from the SITE's own first word and requires
\ the other three lanes to name that same one. ADDR-RD-MASK is the destination-
\ register field of a move-wide word and ADDR-RD-BITS its width, so shifting a
\ scaffold down by that width and back clears the register out of it and leaves the
\ opcode and the shift, which is what the site's register is then put back into.
\ Accepting any register without the agreement requirement would accept four words
\ that name four different registers, whose four immediates spell out no address at
\ all; formal/Common/Reloc.v states both halves.
$FFE0001F constant ADDR-OPC-MASK
$FFFF constant ADDR-IMM-MASK
16 constant ADDR-CHAIN-BYTES
$1F constant ADDR-RD-MASK
5 constant ADDR-RD-BITS

\ ONE READER AND ONE WRITER OF A CHAIN'S VALUE, beside the shape they read it
\ with. Three passes need them - the capture's site scan, the merge that rebases
\ a read-back capture into a host's coordinates, and any later consumer - and a
\ private copy in each is exactly the drift the shape constants moved here to
\ stop. The relocation pass in src/habu/habu2.f is not one of the three: it is
\ EMITTED machine code and consumes the constants directly.
private

\ The chain's own instruction word. Owner-prefixed like every other reader of
\ this shape in the tree (AOT-W32@, ACAP-W32@; see tools/jitdump-core.f).
: W32@ ( ptr u8 -- n ) {: p:ptr :}
   p c@  p 1+ c@ 8 lshift or  p 2 + c@ 16 lshift or  p 3 + c@ 24 lshift or ;

: W32! ( n ptr u8 -- ) {: w:n p:ptr :}
   w p c!  w 8 rshift p 1+ c!  w 16 rshift p 2 + c!  w 24 rshift p 3 + c! ;

public

\ The four immediate fields as one value, whatever register the chain names: the
\ immediate sits just above the destination-register field, which is what
\ ADDR-RD-BITS is the width of.
: CHAINV ( ptr u8 -- n ) {: p:ptr :}
   p W32@ ADDR-RD-BITS rshift ADDR-IMM-MASK and
   p 4 + W32@ ADDR-RD-BITS rshift ADDR-IMM-MASK and 16 lshift or
   p 8 + W32@ ADDR-RD-BITS rshift ADDR-IMM-MASK and 32 lshift or
   p 12 + W32@ ADDR-RD-BITS rshift ADDR-IMM-MASK and 48 lshift or ;

\ Re-encode a full 64-bit value into an existing chain, keeping each word's
\ opcode AND its destination register - it never reads the register it
\ preserves, so a chain the native compiler emitted into an allocator's register
\ rewrites as readily as one the engine emitted into x9.
: SET-CHAIN ( ptr u8 n -- ) {: p:ptr val:n :}
   4 0 ?do
      p i 4 * + W32@ ADDR-OPC-MASK and
      val i 16 * rshift ADDR-IMM-MASK and ADDR-RD-BITS lshift or
      p i 4 * + W32!
   loop ;
;package

package AOT-BUF
public

$300000 constant AOT-BLOB-CAP     \ 3 MiB: the 2.15 MiB full runtime with room to grow
\ Capture storage belongs to the build, not the target DATA heap. A host and a
\ later source-bound writer can both be live without consuming two heap-sized
\ sets of buffers. The existing transient registry owns these mappings.
DYNAMIC-BUFFER BLOB-STORAGE n
: AOT-BLOB-BUF ( -- ptr u8 )
   AOT-BLOB-CAP CELL / BLOB-STORAGE-RESERVE
   0 BLOB-STORAGE BYTE-VIEW ;
variable AOT-BLOB-LEN
\ 16384: the chain needs ~6554 records, and DICT-CAP (32768) is the absolute
\ ceiling a capture window can reach, since the window is a dictionary subrange.
16384 constant AOT-REC-MAX
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
$FFFFFFFE constant WID-QUAL

\ 32768 call sites: the metabuild window carries one BL site per 83 blob bytes,
\ so the chain's 1.15 MiB projects to ~14k sites.
32768 constant AOT-SITE-MAX
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
\ THE WINDOW'S CONTENT TRAVELS AS RUNS, AND THE SPAN IS A NUMBER.
\ The window is a dictionary subrange, so almost all of it is `allot`ed room that
\ nothing has written: the compiler chain's window measures 1,531,045 bytes of
\ span and 32 bytes of content, in four cells, 99.998% zero (dot
\ habu-census-the-captured-fe5f7c49). Storing the span verbatim baked 1.5 MB of
\ zeros into every engine. So what is stored is the NON-ZERO EXTENTS - one 8-byte
\ row of (offset u32, length u32) each, and their bytes concatenated in row order -
\ and the seed zeroes the span and lays the runs into it.
\
\ THE SPAN IS NO LONGER A LENGTH ANYWHERE, which is why AOT-DATA-SIZE is now a
\ genuine scalar in the artifact rather than a section's length. SPAN-CAP bounds
\ it, and it bounds nothing else: the window costs no buffer here now, so the cap
\ exists only to keep the u32 offsets in the two tables below honest and to fail
\ closed on a span no engine could reserve. 16 MiB against the full runtime's
\ measured 8,310,765 bytes leaves honest headroom at no storage cost.
$1000000 constant SPAN-CAP
\ 262144 rows against the full runtime's measured 180710. A run is a maximal non-zero
\ extent, so the count is a property of the window's content and not of its size,
\ and a window that outgrew this is refused by name rather than truncated.
262144 constant RUN-MAX
DYNAMIC-BUFFER RUN-STORAGE n
: RUN-BUF ( -- ptr u8 )
   RUN-MAX RUN-STORAGE-RESERVE
   0 RUN-STORAGE BYTE-VIEW ;
variable RUN-N
\ $100000 against the full runtime's measured 540739 bytes. Overflow is refused by name.
$100000 constant RBYTES-CAP
DYNAMIC-BUFFER RBYTES-STORAGE n
: RBYTES-BUF ( -- ptr u8 )
   RBYTES-CAP CELL / RBYTES-STORAGE-RESERVE
   0 RBYTES-STORAGE BYTE-VIEW ;
variable RBYTES-LEN
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

package AOT-BUF
public

\ boot-run name list: 0-terminated [len][name-bytes] records of the top-level entry
\ words (INSTALL/BPW-INSTALL/S-INSTALL) the metabuild ran at the tail of the REPL
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
create AOT-SIG-BUF AOT-SIG-MAX SIG-ROW * allot    variable AOT-SIG-N
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
: AOT-SIG-BUF@ ( -- ptr u8 ) AOT-SIG-BUF ;
: AOT-SIG-STR-BUF@ ( -- ptr u8 ) AOT-SIG-STR-BUF ;
: AOT-REG-BUF@ ( -- ptr u8 ) AOT-REG-BUF ;

;package

package AOT-XTSITE
public
: BUF@ ( -- ptr u8 ) BUF ;
;package
package AOT-WINDOW
public
: RUN-BUF@ ( -- ptr u8 ) RUN-BUF ;
: RBYTES-BUF@ ( -- ptr u8 ) RBYTES-BUF ;
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

: +ROWS ( n n n -- n ) {: used:n count:n width:n :}
   count 0 < width 0 <= or if REFUSE then
   count AOT-SECTION-CAP width / > if REFUSE then
   used count width * +BYTES ;

\ Fifteen scalar/count cells frame the common baked section. BYTES, rounds
\ each byte run to four bytes; packed rows already have that alignment.
: BODY-BYTES ( -- n )
   15 cells
   AOT-BLOB-LEN @ +BYTES
   AOT-REC-N @ AOT-CREC-ROW +ROWS
   AOT-SITE-N @ SITE-ROW +ROWS
   AOT-NAMES-LEN @ +BYTES
   AOT-DSITE-N @ 4 +ROWS
   AOT-WINDOW:XTOFF-N @ AOT-WINDOW:XTOFF-ROW +ROWS
   AOT-WINDOW:RUN-N @ 8 +ROWS
   AOT-WINDOW:RBYTES-LEN @ +BYTES
   AOT-CSITE-N @ 4 +ROWS
   AOT-XTSITE:N @ 8 +ROWS
   AOT-BOOTRUN-LEN @ 1+ +BYTES
   AOT-PWIN-N @ 4 +ROWS ;

\ The sidecar is emitted as one byte run: its sections have no internal pad.
: PAYLOAD-BYTES ( -- n )
   AOT-SIG-N @ AOT-SIG-STR-LEN @ or AOT-REG-LEN @ or 0= if 0 exit then
   56 AOT-SIG-N @ SIG-ROW +ROWS
   AOT-SIG-STR-LEN @ +RAW AOT-REG-LEN @ +RAW ;

: BYTES ( bool -- n ) {: sidecar:bool :}
   BODY-BYTES
   sidecar if
      8 +BYTES PAYLOAD-BYTES +BYTES
      AOT-SIG:INSTALL-NAME$ nip +BYTES
   then ;

;package
