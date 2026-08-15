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
\ THE CAPS AND THE BUDGET TRAVEL TOGETHER. AOT-SECTION:BYTES sums the buffers
\ below and AGREE executes at load, in every build and every recovery compile,
\ against src/arch/arm64/icode.f AOT-SECTION-CAP. Splitting the budget from the
\ caps it sums is precisely the drift AGREE exists to stop, so nothing here may
\ be moved out on its own: raising a cap must either move the section with it or
\ stop the build.
\
\ WHO LOADS IT. It is a common engine-prefix source, immediately before habu2.f
\ in both builders (tools/bootstrap.sh SRC_COMMON, tools/build-fixpoint.f
\ BF-APPEND-COMMON), named through the stdin-closure manifest
\ (tools/stdin-closure-lib.f SDC-DECL$) so the two lists cannot drift apart, and
\ it loads into a booted bin/hb behind src/arch/arm64/asm.f, icode.f and
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

$200000 constant AOT-BLOB-CAP     \ 2 MiB: the chain's 1.15 MiB of code with room to grow
create AOT-BLOB-BUF AOT-BLOB-CAP allot    variable AOT-BLOB-LEN
\ 16384: the chain needs ~6554 records, and DICT-CAP (32768) is the absolute
\ ceiling a capture window can reach, since the window is a dictionary subrange.
16384 constant AOT-REC-MAX
\ AOT-REC-BUF holds three regions (all viewed via AOT-REC-BUF@, no extra TRUST):
\   [0 .. MAX*48)              verbatim 48B dict records (capture source of truth)
\   [MAX*48 .. +MAX*CREC-ROW)  compact 20B records (three u32 role/code/name fields + metadata + wid)
\   [+MAX*CREC-ROW .. +48)     48B scratch for the build-time expand==verbatim proof
create AOT-REC-BUF AOT-REC-MAX 48 * AOT-REC-MAX AOT-CREC-ROW * + 48 + allot    variable AOT-REC-N
\ 32768 call sites: the metabuild window carries one BL site per 83 blob bytes,
\ so the chain's 1.15 MiB projects to ~14k sites.
32768 constant AOT-SITE-MAX
create AOT-SITE-BUF AOT-SITE-MAX 8 * allot    variable AOT-SITE-N   \ packed 8B rows: blob-off u32 + name-off u32
create AOT-NAMES-BUF AOT-NAMES-CAP allot    variable AOT-NAMES-LEN
\ DATA-literal relocation table (third relocation class): blob offsets of the
\ movz/movk x9 DATA-address literals (create/variable buffer refs). AOT-DATA-D0 =
\ the capture engine's REPL-DATA base (abs); AOT-DATA-SIZE = the REPL DATA span
\ (all allot/variable => zero content). EM-SEED-AOT reserves DATA and rebases each
\ literal by (seed-DP - AOT-DATA-D0).
\ 16384 shared rows: the metabuild window records one address chain per 127 blob
\ bytes, so the chain's 1.15 MiB projects to ~9k DATA plus CODE sites together.
16384 constant AOT-DSITE-MAX
create AOT-DSITE-BUF AOT-DSITE-MAX 4 * allot    variable AOT-DSITE-N   \ packed u32 blob offsets (DATA then CODE)
variable AOT-DATA-D0    variable AOT-DATA-SIZE
\ The window's wordlist span, [W0, W0+SPAN). A captured wid is a window-relative
\ coordinate the same way a blob offset is: only wid 0, the global wordlist, means
\ the same thing in two processes, so the seed maps an in-window wid to
\ T0 + (wid - W0) and refuses every other non-zero wid by name.
variable AOT-WID-W0    variable AOT-WID-SPAN
\ The window's DATA CONTENT, and the offsets the seed must not take from it.
\ Reserving the span zeroed was right only while every byte in it was zero. It is
\ not: the REPL sources put real bytes there - a TRUST row's name and signature
\ are `s"` literals interned into DP - and those arrived in the seeded engine as
\ zeros. So the span travels as bytes, sized by AOT-DATA-SIZE (one authority for
\ the length; the buffer never carries more than the span).
\ A DECLARED ADDRESS CELL IS THE ONE THING THE BYTES MAY NOT CARRY. `defer` in the
\ window allots a dispatch cell and registers it (SNAP-RELOC's XTCELL table), and
\ what it holds is a code address in the BUILDING host - ASLR-varying on macOS.
\ Baking that would make the image depend on the run that built it and the byte
\ fixpoint would never close, which is the same reason the code literals are
\ stored b0-relative. Those cells are therefore zeroed in the image and their
\ offsets listed here; EM-AOT-RELOC-DATA stores the seeded engine's own
\ `defer-unset` trap xt into each one at boot. The offsets are u32, so the window
\ they index is bounded by DATA-CAP alone.

;package

package AOT-WINDOW
public
\ 2 MiB, and the term that made it move: the compiler chain's window measures
\ 1,531,272 DATA bytes, so the 1 MiB this held refused the chain outright ("DATA
\ window exceeds the AOT data buffer"). The metabuild REPL window that sized the
\ old cap measures 5724. Raising it raises AOT-SECTION:BYTES by the same $100000,
\ which is why icode.f AOT-SECTION-CAP and CODE-CAP-BYTES move with it and why
\ AGREE below is executed rather than commented: the three are one number.
$200000 constant DATA-CAP
create DATA-BUF DATA-CAP allot
4096 constant XTOFF-MAX          \ declared address cells in the window (metabuild REPL: 1)
create XTOFF-BUF XTOFF-MAX 4 * allot    variable XTOFF-N   \ packed u32 window offsets
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
\ WHY THIS EXISTS BEFORE ITS PRODUCER. A code literal that names a PRE-WINDOW word
\ cannot be rebased: its correct value is fixed by the prefix's own layout and
\ differs between the metabuild host and bin/hb, which is why ACAP-SCAN-DSITES
\ still refuses one. Resolving it by name is the answer, and the row is the format
\ half of that answer; the pass that decides WHICH sites become named rows is the
\ inliner-decline work, dot habu-aot-pre-window-0b01043c. The format is baked into
\ the engine, so it migrates once - a row kind added later is a second migration
\ of every baked-code route. In-window code literals stay b0-relative: rebasing
\ them is correct and costs no lookup.

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

\ protected-WID registry AOT image (TFAM 2b-v): a bit-for-bit image of the live
\ friend-arena bitmap, baked so EMIT-AOT-PROT-RESTORE can restore it at boot --
\ advancing WIDN past the highest restored WID so a post-restore wordlist alloc
\ cannot collide with a protected one. The blob is a fixed PROT-BITS-BYTES image of
\ a SET, so it does not depend on the order the writing run protected its WIDs. It
\ is always emitted at full width, which is what lets the shape tag in front of it
\ be the frame's version.
create AOT-PWID-BUF PROT-BITS-BYTES allot

\ The window's own protected WIDs, window-relative, kept OUT of the bitmap above.
\ EMIT-AOT-PROT-RESTORE runs before the cold prefix and so before the target's wid
\ base exists; only these rows can carry a window wordlist's seal across, and the
\ seed sets each bit after rebasing it. The chain's window seals 125.
4096 constant AOT-PWIN-MAX
create AOT-PWIN-BUF AOT-PWIN-MAX 4 * allot    variable AOT-PWIN-N   \ packed u32

\ Raw emitter-boundary views (same pattern as SRCA@): expose the build-scratch
\ buffers as `ptr` for the checked copy/BYTES, sites below.
\ The blob, record, site, name, relocation, and boot-run accessors refine their
\ respective scratch buffers.
: AOT-BLOB-BUF@ ( -- ptr u8 ) AOT-BLOB-BUF ;
s" AOT-BUF:AOT-BLOB-BUF@" s" -- ptr u8" TRUST
: AOT-REC-BUF@ ( -- ptr a ) AOT-REC-BUF ;
s" AOT-BUF:AOT-REC-BUF@" s" -- ptr a" TRUST
: AOT-SITE-BUF@ ( -- ptr u8 ) AOT-SITE-BUF ;
s" AOT-BUF:AOT-SITE-BUF@" s" -- ptr u8" TRUST
: AOT-NAMES-BUF@ ( -- ptr u8 ) AOT-NAMES-BUF ;
s" AOT-BUF:AOT-NAMES-BUF@" s" -- ptr u8" TRUST
: AOT-DSITE-BUF@ ( -- ptr u8 ) AOT-DSITE-BUF ;
s" AOT-BUF:AOT-DSITE-BUF@" s" -- ptr u8" TRUST

;package

package AOT-XTSITE
public
: BUF@ ( -- ptr u8 ) BUF ;
s" AOT-XTSITE:BUF@" s" -- ptr u8" TRUST
;package
package AOT-WINDOW
public
: DATA-BUF@ ( -- ptr u8 ) DATA-BUF ;
s" AOT-WINDOW:DATA-BUF@" s" -- ptr u8" TRUST
: XTOFF-BUF@ ( -- ptr u8 ) XTOFF-BUF ;
s" AOT-WINDOW:XTOFF-BUF@" s" -- ptr u8" TRUST
;package

package AOT-BUF
public

: AOT-BOOTRUN-BUF@ ( -- ptr u8 ) AOT-BOOTRUN-BUF ;
s" AOT-BUF:AOT-BOOTRUN-BUF@" s" -- ptr u8" TRUST
\ Retirement for the six accessors above: habu-builder-trust-rows-c5d41af6.
: AOT-PWID-BUF@ ( -- ptr u8 ) AOT-PWID-BUF ;
s" AOT-BUF:AOT-PWID-BUF@" s" -- ptr u8" TRUST
: AOT-PWIN-BUF@ ( -- ptr u8 ) AOT-PWIN-BUF ;
s" AOT-BUF:AOT-PWIN-BUF@" s" -- ptr u8" TRUST

;package


\ ---- how much window the AOT section can ask for --------------------------
\ THE THIRD TERM OF THE CODE WINDOW IS OWNED HERE, because the buffers are here.
\ src/arch/arm64/icode.f sizes CODE-CAP-BYTES as ADR-HI + IBUFSZ +
\ AOT-SECTION-CAP and cannot see any of these caps -- it is compiled first, and
\ the recovery host reads it first too. So the sum is derived here from the
\ buffers themselves and the agreement is EXECUTED at load, in every build and
\ every recovery compile: raising a cap without the window stops the build
\ instead of shipping an emitter whose buffer cannot hold what it may bake. That
\ is the shape src/habu/rt.f RT:DSTACK-AGREE already uses for mnem.f's XDS and
\ layout.f's ENGINE-GPR:DSTACK.
\ The rounding is the section's headroom for what is not a buffer: the twelve
\ count cells that head the tables and the pad each BYTES, run takes to the next
\ 4-byte boundary, at most a couple of hundred bytes against 46 KiB of grain. It
\ is a belt in any case -- a section that outgrew this would be refused by the
\ emitter's own `icode: code buffer overflow`, and each buffer refuses on its own
\ overflow long before that.
package AOT-SECTION

\ the sum below reads the caps by their bare names; the import closes with the package.
using AOT-BUF

public

$10000 constant GRAIN                              \ 64 KiB, the largest target page
AOT-BLOB-CAP
AOT-REC-MAX AOT-CREC-ROW * +                       \ compact dictionary records
AOT-SITE-MAX 8 * +                                 \ call-site rows
AOT-NAMES-CAP +                                    \ deduped name pool
AOT-DSITE-MAX 4 * +                                \ DATA-literal sites
AOT-DSITE-MAX 4 * +                                \ CODE-literal sites (same buffer's tail)
AOT-WINDOW:XTOFF-MAX 4 * +                         \ declared address cells in the window
AOT-WINDOW:DATA-CAP +                              \ the captured DATA window's content
AOT-XTSITE:MAX 8 * +                               \ named code-literal rows
AOT-BOOTRUN-CAP 1 + +                              \ +1 = the live 0 terminator
PROT-BITS-BYTES +                                  \ protected-WID bitmap image
AOT-PWIN-MAX 4 * +                                 \ the window's own protected WIDs
GRAIN 1 - + GRAIN / GRAIN *
constant BYTES

private

: AGREE ( -- )
   BYTES AOT-SECTION-CAP <>
   if s" habu2: AOT section caps and icode.f AOT-SECTION-CAP disagree" ICODE-EXIT-RC die then ;
AGREE

;package