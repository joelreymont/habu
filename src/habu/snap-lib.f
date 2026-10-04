\ snap-lib.f — checked snapshot image writer definitions.
\
\ Load after target image emission words (`BUILD-SNAP-HDR`, `SNAP-DROP`,
\ `SNAP-EXTRA-PTR`, `SNAP-EXTRA-SIZE`). Entry files decide when
\ to prepare checker/include state and call the writer's `PERSIST`.
\
\ Everything here belongs to package SNAP. The only word an entry file needs is
\ the public `SNAP:PERSIST`. The live LOWER-CERT-HOOK installed by the core
\ prefix survives capture and restore; the writer does not replace it. Writer
\ state and scratch-copy machinery stay package-private.
\
\ The entry is `SNAP:PERSIST` - it builds the header, canonicalises the two
\ regions, writes and signs the image beside its output path and renames it
\ over that path, then exits. The tail is deliberately not `GO`:
\ several other files already define a `GO`, and the name says nothing about
\ what the word does. src/habu/app-image-core.f APP-IMAGE:SAVE calls the entry.

require lib/fs.f
require lib/fs-mutate.f
require lib/codesign.f
require src/habu/address-cells.f
require src/habu/snapshot-format.f
require src/habu/cell-grid.f
require src/habu/fdio.f
require src/habu/sign-id.f
require src/habu/stack-abi.f

package SNAP

create OUTPUT FS-PATH-CAP allot
variable OUTPUT-U

\ Every persist names its output first (PATH!), copied into DATA rather than
\ retained in argv. The image stores none of it (SND-ZERO-WRITER), so a restored
\ process that persists without naming its own path is refused instead of
\ writing where its build did.
: OUT-PATH ( -- ptr u8 n ) OUTPUT OUTPUT-U @ ;

: OUT-PATH-NAMED ( -- )
   OUTPUT-U @ 0= if s" snap: persist has no output path" 74 die then ;

\ Snapshot trailer format version (item 12 slice 3b, dot
\ habu-snapshot-format-ver): once 3b bakes nonzero hidden-field counts into the
\ persisted effect-node arena, a pre-3b engine restoring such an image would
\ misread hidden fields as logical params. The trailer grows 40->48 bytes with
\ a format-version cell at offset 40; the loader (habu2.f EM-SNAPSHOT-RESTORE)
\ rejects any image whose version is not the current format with a distinct
\ exit status (80, mirroring the snbad rc-79 corrupt-trailer path). Bump on any
\ layout change that a prior engine would misread.
\ Version 2: dict record [16] bits 52-59 became the DNAME-MIN-IN certified
\ input-arity band (dot habu-habu-certified-words-84e84eaf); a pre-change
\ engine restoring a post-change snapshot would fold the band into name
\ lengths (its length reads clear only the top 4 bits), so it must fail
\ closed rc 80 instead of misreading the dictionary.
\ Version 3: snapshot DATA includes the owner public/private WID registry.
\ Older formats cannot prove qualified-call visibility and fail closed.
create TRL SNAP-TRL-BYTES allot
variable STB  variable STSZ  variable SDB  variable SCL  variable SDL
variable SRL  variable SML  variable SIL  variable SDW
variable SNL  variable SFTS  variable SPAD  variable SFD
variable SHF  variable SHL           \ heap form (SNAPSHOT-FORMAT:HEAP-RAW/-GRID), stored heap bytes
variable SXT-N                       \ x86 text scratch, never persisted as a pointer
create PAD-ZEROS 16 allot
\ These views expose the raw snapshot source and dictionary/data buffer cells.
\ Retirement: habu-campaign-c2-mem-c3d7662b.
: STB@ STB @ ;
s" STB@" s" -- ptr u8" TRUST
: STB-CELL@ STB @ ;
s" STB-CELL@" s" -- ptr n" TRUST
: SDB@ SDB @ ;
s" SDB@" s" -- ptr u8" TRUST

: SIZE! ( -- )
   STSZ @ SRL @ + SDW @ + SNAP-TRL-BYTES + SNL ! ;

: HDR! ( -- snap )
   SNL @ BUILD-SNAP-HDR SFTS ! ;

: PAD! ( -- snap )
   HDR!
   SFTS @ CODE-OFF - SNL @ - SPAD ! ;

: STALE ( snap -- )
   SNAP-DROP ;

: ABSORB-PAD ( -- snap )
   SIZE!
   PAD! STALE
   SPAD @ 0 < SPAD @ PROT-PAGE-MAX >= or if
      s" snap: invalid image alignment padding" 74 die then
   SDW @ SPAD @ + SDW !
   SIZE!
   HDR! ;

: RESET-BUF ( -- )
   \ MBUF-A is a process-local mmap pointer; restored images must allocate
   \ their own buffer before emitting a fresh ELF/snapshot header.
   NULL-PTR MBUF-A !
   NULL-PTR MP !
   0 MLEN! ;

: BAD-SOURCE ( -- )
   s" snap: invalid source image extent" 74 die ;

: TEXT-CUT ( n n -- n ) {: bytes:n cut:n :}
   cut 0 < cut bytes > or if BAD-SOURCE then
   bytes cut - ;

\ A restored snapshot's authenticated text contains its old region, DATA and
\ trailer. Copy only the prefix preceding those payloads; otherwise every
\ recapture embeds the previous snapshot inside the next one. The loader has
\ already validated this trailer. Do not guess at older trailers inside the
\ prefix of an image produced by a previous, nesting writer.
: ENGINE-TEXT-SIZE ( -- n )
   STB-CELL@ CODE-OFF - IMAGE-TEXT-SIZE-OFF + @
   IMAGE-TEXT-CONTENT-ADJ - {: bytes:n :}
   data-base SNAP-CELL + CELL-VIEW @ 0= if bytes exit then
   bytes SNAP-TRL-BYTES TEXT-CUT {: end:n :}
   STB@ end + {: trailer:ptr :}
   trailer CELL-VIEW @ SNAP-MAGIC <> if BAD-SOURCE then
   HB-TARGET-LINUX-X86-64? if
      trailer SNAP-TRL-DATALEN + CELL-VIEW @ DATA-START -
         end swap TEXT-CUT exit
   then
   end trailer SNAP-TRL-NDICT + CELL-VIEW @ DREC *
      trailer SNAP-TRL-REGLEN + CELL-VIEW @ DICT-SIZE - + TEXT-CUT
   trailer SNAP-TRL-DATALEN + CELL-VIEW @ TEXT-CUT ;

: GEOMETRY ( -- )
   RESET-BUF
   \ The builder's x20 register constant is XREG-RBASE so it does not shadow
   \ the `rbase` primitive; read the saved text base straight from its cell.
   data-base RBASE-CELL + @ STB !         \ text CONTENT base
   ENGINE-TEXT-SIZE STSZ !
   dbase@ SDB !
   cp@ SDB @ - SCL !                      \ virtual region extent
   here data-base - SDL !                 \ exact virtual DATA extent
   ndict@ DREC * SCL @ DICT-SIZE - + SRL !
   SCL @ 31 + 32 / DICT-SIZE 32 / - SML !
   data-base BYTE-VIEW SDL @ ADDRESS-CELLS:DATA-SPAN nip
   data-base BYTE-VIEW SNAP-RELOC:XTCELL-N-CELL + CELL-VIEW
      ADDRESS-CELLS:BASE-FIELD + @ ADDRESS-CELLS:BOOT-OFF =
      if cells else drop 0 then SIL ! ;

\ The stored DATA stream's length waits for the heap section's form, which the
\ writer chooses from the canonical copy (ENCODE-HEAP below).
: FRAME ( -- snap )
   8 SNAP-RELOC:CALLMAP-OFF + SML @ 2 * +
      ADDRESS-CELLS:HEADER-BYTES + SIL @ + SHL @ + SDW !
   ABSORB-PAD ;


\ ---- canonical-base persistence ----
\ Snapshot images must be byte-identical across ASLR runs, so the dict/code
\ region is COPIED to scratch and the copy is rebased to canonical base 0
\ with the same engine relocation walks the startup loader uses
\ (snap-rebase primitive -> LSNAPRBD, src/habu/habu2.f). The live
\ region is never touched: rewriting it in place would break the very call
\ chains this writer executes. The trailer records base 0; the loader's
\ delta and membership math are base-agnostic, so restore needs no change.
$1002 constant SNC-MAP-ANON
3 constant SNC-PROT-RW

variable SNC-N

\ Scratch region view: raw anonymous mmap address held as a cell; the
\ typed view is the one audited reinterpret (same class as IMGD-MMAP-PTR).
\ All scratch views, zeroers, and quarantine-table reads share one owner.
\ Retirement: habu-sweep-trusted-out-41e973ce.
TRUSTED: SNC-PTR ( -- ptr u8 ) SNC-N @ ;
TRUSTED: SNC-TEXT-N ( -- n ) STB @ ;

: SNC-ALLOC ( -- )
   SNC-N @ 0 <> if exit then
   0 SCL @ SNC-PROT-RW SNC-MAP-ANON -1 0 mmap
   dup 0 < if s" snap: scratch mmap failed" 74 die then
   SNC-N ! ;

: SNC-COPY ( -- )
   SDB@ SNC-PTR SCL @ BYTE-COPY ;

: SNC-CANON ( -- )
   HB-TARGET-LINUX-X86-64? if
      ndict@ DREC * {: used:n :}
      DICT-SIZE used - 0 ?do 0 SNC-PTR used i + + c! loop
      exit
   then
   SNC-N @  SNC-N @ SCL @ +  ndict@
   SNC-TEXT-N  STSZ @  0
   snap-rebase ;


\ Live-state cells inside the persisted DATA region differ per run (ASLR
\ text base, stack, argv, cached label addresses) and are all overwritten
\ by the loader/startup (EM-SNAPSHOT-RESTORE + EM-STARTUP-RUNTIME-STATE,
\ src/habu/habu2.f). Zero them in a scratch copy so images are
\ byte-identical. tools/hb-build-repl-twin-test.f builds one program twice
\ and compares the images, so a new live cell left out of this list fails
\ that row loudly.
variable SND-N

TRUSTED: SND-PTR ( -- ptr u8 ) SND-N @ ;

: SND-ALLOC ( -- )
   SND-N @ 0 <> if exit then
   0 SDL @ SNC-PROT-RW SNC-MAP-ANON -1 0 mmap
   dup 0 < if s" snap: data scratch mmap failed" 74 die then
   SND-N ! ;

TRUSTED: SND-ZERO-CELL ( n -- )
   SND-N @ + 0 swap ! ;

\ The return-stack window used to live at RSTK-OFF..RSTK-END inside this image
\ and had to be zeroed: stale slots held dangling arena pointers from the build
\ (proven: two old-USIGS pointers survived there). It is a guarded mapping
\ outside DATA now, so the image carries none of it -- only the base cell, which
\ SND-ZERO-LIVE clears with the other per-process addresses.

: SND-ZERO-LIVE ( -- )
   RBASE-CELL SND-ZERO-CELL   S0-CELL SND-ZERO-CELL
   STACK-ABI:CAP-CELL SND-ZERO-CELL
   STACK-ABI:REPL-BASE-CELL SND-ZERO-CELL
   STACK-ABI:REPL-CAP-CELL SND-ZERO-CELL
   STACK-ABI:RETURN-BASE-CELL SND-ZERO-CELL
   STACK-ABI:LOOP-BASE-CELL SND-ZERO-CELL
   ARGC-CELL SND-ZERO-CELL    ARGV-CELL SND-ZERO-CELL
   ENVP-CELL SND-ZERO-CELL    SNAP-CELL SND-ZERO-CELL
   HND-CELL SND-ZERO-CELL     PEND-CELL SND-ZERO-CELL
   PKG-PUB-CELL SND-ZERO-CELL PKG-PRI-CELL SND-ZERO-CELL
   PKG-PARENT-CELL SND-ZERO-CELL PKG-REC-CELL SND-ZERO-CELL
   LOOPSP-CELL SND-ZERO-CELL  DOESP-CELL SND-ZERO-CELL
   \ ARM startup reinstalls LCREATE. The x86-64 fixed-address image keeps its
   \ captured Habu CREATE handler, which snapshot startup does not reinstall.
   HB-TARGET-LINUX-X86-64? 0= if CREATEP-CELL SND-ZERO-CELL then
   RRECP-CELL SND-ZERO-CELL
   LMAINP-CELL SND-ZERO-CELL  DOESB-CELL SND-ZERO-CELL
   EVALREC-CELL SND-ZERO-CELL UNCGH-CELL SND-ZERO-CELL
   REFUSAL-ABI:CODE-CELL SND-ZERO-CELL
   SIGNAL-ABI:STUB-CELL SND-ZERO-CELL
   HB-TARGET-LINUX-X86-64? 0= if
      AOT-CELLS:SPAN-TABLE-CELL SND-ZERO-CELL
      AOT-CELLS:SPAN-N-CELL SND-ZERO-CELL
      AOT-CELLS:SPAN-BASE-CELL SND-ZERO-CELL
   then
   TSIG-A-CELL SND-ZERO-CELL  TSIG-U-CELL SND-ZERO-CELL
   TCSIG-A-CELL SND-ZERO-CELL TCSIG-U-CELL SND-ZERO-CELL
   CRSIG-A-CELL SND-ZERO-CELL CRSIG-U-CELL SND-ZERO-CELL
   INP-CELL SND-ZERO-CELL     INE-CELL SND-ZERO-CELL
   HIDXP-CELL SND-ZERO-CELL
   HIDX:CLAIMS SND-ZERO-CELL
   ADDRESS-CELLS:INDEX-CELL SND-ZERO-CELL
   TKA-CELL SND-ZERO-CELL     TKL-CELL SND-ZERO-CELL
   PENDTKA-CELL SND-ZERO-CELL
   DEF-TKA-CELL SND-ZERO-CELL
   AOT-SEED-DONE-CELL SND-ZERO-CELL
   BOOT-SRC:USER-END SND-ZERO-CELL
   EVAL-TOP-CELL SND-ZERO-CELL  CLOSED-FREE-CELL SND-ZERO-CELL
   FLOORREC-CELL SND-ZERO-CELL
   HB-TARGET-LINUX-X86-64? 0= if CODE-END-CELL SND-ZERO-CELL then
   NCOMP-DISPATCH:DEF-TIER-CELL SND-ZERO-CELL
   NCOMP-DISPATCH:BUILD-DEPTH-CELL SND-ZERO-CELL
   NCOMP-DISPATCH:BUILD-TIER-CELL SND-ZERO-CELL ;

\ Admissions and the seal belong to the writing process. A saved image starts
\ with a fresh vocabulary even when its writer was sealed during capture.
: SND-ZERO-POLICY ( -- )
   POLICY-NDICT-CELL SND-ZERO-CELL
   PROT-BITS-BYTES 0 ?do POLICY-BITS-OFF i + SND-ZERO-CELL CELL +loop ;

: SND-COPY ( -- )
   data-base SND-PTR SDL @ BYTE-COPY ;

\ ---- persisted cells that hold a JIT-region address --------------------------
\ Everything inside the region copy is already canonicalised: pointers into the
\ region are folded to the RBASE-VA sentinel and call displacements to the
\ canonical REGION-OFF distance. Some cells in DATA hold region addresses too --
\ every deferred word's dispatch cell, the three engine hooks and the compiler
\ dispatch cell -- and DATA
\ is copied verbatim, so before this they arrived at a restoring run still
\ pointing at the writing run's region.
\ That was survivable only while the region had a fixed address. It is not now:
\ the loader takes whatever base the kernel gives it (dot
\ habu-relocate-snapshot-region-752042fe), so a stale cell is wrong in every run.
\ Measured under lldb on a restored image before this: `ldr x16,[x9]` then
\ `blr x16` in a compiled deferred call jumped to 0x105a1dd30, the writing run's
\ address for the target, with the live region at 0x103550000 -- an immediate
\ SIGSEGV on the first deferred call.
\ Which cells those are is never guessed from what a cell contains: an ordinary
\ integer may hold any value, including one that looks exactly like a region
\ address. The engine declares each cell where its kind is decided -- `defer` when
\ it allocates a dispatch cell, `is` when it stores into one, and cold boot for
\ the fixed hook/dispatch cells -- and records the DATA offset in the table this pass
\ walks. The loader (habu2.f EM-SNAPSHOT-RESTORE) inverts exactly this list from
\ exactly the same table.
\ These four words belong to this package, the snapshot writer, rather than to
\ SNAP-RELOC: the engine owns the declaring and the restoring, and the writer owns
\ the one pass that runs over its own scratch copy. They read the table's shape
\ from SNAP-RELOC and nothing else.
TRUSTED: SND-XT-CELL@ ( n -- n ) SND-N @ + @ ;
TRUSTED: SND-XT-CELL! ( n n -- ) SND-N @ + ! ;

: SND-ROWS ( -- ptr n n )
   ADDRESS-CELLS:CURRENT? if SND-PTR SDL @ ADDRESS-CELLS:DATA-SPAN exit then
   SND-PTR SNAP-RELOC:XTCELL-ROWS-OFF + cell-view
   SNAP-RELOC:XTCELL-N-CELL SND-XT-CELL@ ;

: SND-XT-ROW ( n -- n ) {: row:n :}
   SND-ROWS drop row cells + @ ;

: SND-XT-OFF ( n -- n ) SNAP-RELOC:XTCELL-OFF-MASK and ;

: SND-XT-DATA? ( n -- bool ) SNAP-RELOC:XTCELL-DATA-TAG and 0 <> ;

\ The offset is about to index this writer's scratch copy of DATA, so it has to
\ name a whole cell inside DATA before it is used for anything. The same band the
\ declaration and the loader enforce; refusing here means a bad row can never
\ reach an image, and the writer never reads or writes outside its own copy.
\ The unsigned bound is split in two because this side is checked Habu with
\ signed cells: a negative offset fails the first test rather than wrapping.
\ Containment only -- alignment is deliberately not required (src/habu/layout.f).
: SND-XT-CELL-OK? ( n -- bool ) {: cell:n :}
   cell 0 < if 0 0= 0= exit then
   cell SNAP-RELOC:XTCELL-OFF-MAX > 0= ;

\ Kept as its own word so the guarded body above stays a clean token run for the
\ relocation manifest, which freezes these bodies exactly (test/compiler).
: SND-XT-CELL-REFUSE ( -- )
   s" snap: declared address cell outside DATA" SNAP-RELOC:XTBAND-RC die ;

: SND-CANON-XT-CELL ( n -- ) {: row:n :}
   row SND-XT-OFF {: cell:n :}
   cell SND-XT-CELL-OK? 0= if SND-XT-CELL-REFUSE then
   row SND-XT-DATA? if exit then
   cell SND-XT-CELL@ {: xt:n :}
   xt 0= if exit then
   xt dbase@ - RBASE-VA +  cell SND-XT-CELL! ;

: SND-CANON-XT-CELLS ( -- )
   SND-ROWS {: rows:ptr count:n :}
   count 0 ?do rows i cells + @ SND-CANON-XT-CELL loop ;

\ The DATA offset of a cell is a BYTE distance, so both ends are taken as byte
\ pointers before the subtraction. `MBUF-A` is a PTR-VARIABLE, so the bare
\ `MBUF-A data-base -` asked `-`'s `ptr a ptr a -- n` row to make the DATA base
\ the address of an ADDRESS, which is the one thing a base address never is
\ (dot habu-fence-a-base-c6c1d71d). `BYTE-VIEW` is the same door the AOT linker's
\ DATA-PTR already takes: it names what the distance is measured in and says
\ nothing about what either cell holds.
: SND-ZERO-OFF ( ptr a -- n )
   BYTE-VIEW data-base BYTE-VIEW - ;

: SND-ZERO-WRITER ( -- )
   SNC-N SND-ZERO-OFF SND-ZERO-CELL
   SND-N SND-ZERO-OFF SND-ZERO-CELL
   SXT-N SND-ZERO-OFF SND-ZERO-CELL
   MBUF-A SND-ZERO-OFF SND-ZERO-CELL
   MP SND-ZERO-OFF SND-ZERO-CELL
   MLEN SND-ZERO-OFF SND-ZERO-CELL
   STB SND-ZERO-OFF SND-ZERO-CELL
   SDB SND-ZERO-OFF SND-ZERO-CELL
   SFD SND-ZERO-OFF SND-ZERO-CELL
   OUTPUT-U SND-ZERO-OFF SND-ZERO-CELL
   OUTPUT SND-ZERO-OFF {: out:n :}
   FS-PATH-CAP 0 ?do out i + SND-ZERO-CELL CELL +loop ;

\ PERSIST admitted the complete retained code interval before copying. Freeze
\ exactly that interval, excluding abandoned rows and the old engine's ASLR
\ coordinates. The restoring engine supplies its own primitive-text evidence.
: SND-CANON-ORIGIN ( -- )
   \ These are byte offsets into the scratch copy, not pointer values.
   TIER-PROV:OPEN-CELL
   begin dup TIER-PROV:END < while dup SND-ZERO-CELL CELL + repeat drop
   SCL @ DICT-SIZE <= if exit then
   1 TIER-PROV:N-CELL SND-XT-CELL!
   DICT-SIZE TIER-PROV:TABLE-OFF SND-XT-CELL!
   SCL @ TIER-PROV:TABLE-OFF CELL + SND-XT-CELL!
   1 TIER-PROV:TABLE-OFF 2 cells + SND-XT-CELL! ;

: CANON-DATA ( -- )
   SND-ALLOC
   DYNAMIC-STORAGE:RELEASE-ALL
   \ Allocation order does not establish liveness: the native string pool
   \ precedes IMK-NDICT0. Owners retire transient state before this copy.
   SND-COPY
   SND-ZERO-LIVE
   SND-ZERO-POLICY
   SND-ZERO-WRITER
   SND-CANON-ORIGIN
   HB-TARGET-LINUX-X86-64? 0= if SND-CANON-XT-CELLS then ;

: CANON-REGION ( -- )
   SNC-ALLOC
   SNC-COPY
   SNC-CANON ;

\ ---- the heap section: its bytes, or the cell grid when that is smaller ------
\ Everything the stream restores above DATA-START is heap: tables `allot`ed at
\ their capacity, registries, arenas and the application's own storage. Most
\ of it is zero, and in the grid form (src/habu/cell-grid.f) a zero cell costs
\ one bitmap bit, or nothing when its whole group is zero. The writer measures
\ both forms on the canonical copy and stores the grid only when it is
\ smaller, so a dense heap is never enlarged; the trailer's heap field records
\ the choice and src/habu/habu2.f EM-SNAPSHOT-DECODE-HEAP lays the grid over
\ zeros. This omits zero bytes from the file; it removes no allocation, and
\ the restored heap is the same extent, byte for byte.
\ The grid cannot be measured before the copy exists, so it runs after
\ CANON-DATA, and its scratch is mapped at the size of what it holds.
variable SBL                         \ flat bitmap bytes, ending at the last present cell's byte
variable SVL                         \ value bytes of the present cells
variable SBM-N                       \ flat bitmap scratch
variable SGR-N                       \ grid stream scratch

TRUSTED: SBM-PTR ( -- ptr u8 ) SBM-N @ ;
TRUSTED: SGR-PTR ( -- ptr u8 ) SGR-N @ ;

: SCRATCH ( n -- n ) {: bytes:n :}
   0 bytes SNC-PROT-RW SNC-MAP-ANON -1 0 mmap
   dup 0 < if s" snap: heap scratch mmap failed" 74 die then ;

: HEAP-BYTES ( -- n ) SDL @ DATA-START - ;

: HEAP-CELLS ( -- n )
   HEAP-BYTES CELL-GRID:CELL-BYTES 1- + CELL-GRID:CELL-BYTES / ;

\ One heap cell of the canonical copy. The grid rounds the heap up to whole
\ cells, and the bytes that adds lie above the exact extent: they read as zero,
\ and the loader refuses a cell that says otherwise.
: HEAP-CELL@ ( n -- n ) {: c:n :}
   DATA-START c CELL-GRID:CELL-BYTES * + {: off:n :}
   SND-PTR off + CELL-VIEW @ {: v:n :}
   SDL @ off - {: room:n :}
   room CELL-GRID:CELL-BYTES >= if v exit then
   v  1 room 8 * lshift 1-  and ;

: BM-SET ( n -- ) {: c:n :}
   SBM-PTR c CELL-GRID:CELL-BITS / + {: at:ptr :}
   at c@  1 c CELL-GRID:CELL-BITS mod lshift or  at c! ;

\ The flat bitmap, the value bytes and the bitmap's length, in one pass.
: HEAP-SCAN ( -- )
   0 SVL !  0 SBL !
   HEAP-CELLS 0 ?do
      i HEAP-CELL@ {: v:n :}
      v 0<> if
         i BM-SET
         SVL @ v CELL-GRID:CELL-VLEN + SVL !
         i CELL-GRID:CELL-BITS / 1+ SBL !
      then
   loop ;

: GRID-BYTES ( -- n )
   SBL @ CELL-GRID:GROUPS CELL-GRID:PMAP-BYTES
   SBM-PTR SBL @ CELL-GRID:STORED-BYTES +
   SVL @ + SNAPSHOT-FORMAT:GRID-FRAME + ;

: GRID-VALUES ( ptr u8 -- ) {: at:ptr :}
   0  HEAP-CELLS 0 ?do
      i HEAP-CELL@ {: v:n :}
      v 0<> if  v over at + CELL-GRID:CELL-V! +  then
   loop drop ;

: WRITE-GRID ( -- )
   SHL @ SCRATCH SGR-N !
   SBM-PTR SBL @ SGR-PTR SNAPSHOT-FORMAT:GRID-FRAME + CELL-GRID:COMPACT {: groups:n stored:n :}
   groups SGR-PTR CELL-VIEW !
   stored SGR-PTR 8 + CELL-VIEW !
   SGR-PTR SNAPSHOT-FORMAT:GRID-FRAME + groups CELL-GRID:PMAP-BYTES + stored + GRID-VALUES ;

: ENCODE-HEAP ( -- )
   SNAPSHOT-FORMAT:HEAP-RAW SHF !  HEAP-BYTES SHL !
   HEAP-CELLS 0= if exit then
   HEAP-CELLS CELL-GRID:CELL-BITS 1- + CELL-GRID:CELL-BITS / SCRATCH SBM-N !
   HEAP-SCAN
   GRID-BYTES {: grid:n :}
   grid HEAP-BYTES >= if exit then
   SNAPSHOT-FORMAT:HEAP-GRID SHF !  grid SHL !
   WRITE-GRID ;


\ The final-close fault hook: WRITE-BYTES runs it on the output fd just before
\ the close it checks, and it does nothing. It is private, so no qualified name
\ reaches it; test/snapshot-writer-close-fail.f reopens this package to make it
\ close the fd early, which proves WRITE-BYTES fails closed (rc 74) and leaves
\ the output path as it was.
defer BEFORE-CLOSE ( n -- )

: CLOSE-NOOP ( n -- )
   drop ;

: CLOSE-DEFAULT ( -- )
   [: CLOSE-NOOP ;] is BEFORE-CLOSE ;

CLOSE-DEFAULT

\ The image is written to a sibling of the output path and renamed over it only
\ after the write, the close and the signature succeed, so a failure never
\ leaves a partial or unsigned executable where a later run finds it. The
\ sibling is created exclusively under a per-process name (lib/fs-mutate.f
\ RESERVE-SIBLING), so a concurrent writer of the same output stages its own.
\ It is chosen after CANON-DATA copies DATA, so the image never holds its name.
create STAGED FS-PATH-CAP allot
variable STAGED-U

: STAGED-PATH ( -- ptr u8 n ) STAGED STAGED-U @ ;

\ Registered for removal at exit before a byte is written, so every `die` and
\ uncaught throw from here to the rename takes the sibling with it
\ (lib/fs-mutate.f CLEANUP-AT-EXIT); after the rename the path names nothing and
\ the removal skips it. A full registry refuses the claim, and the empty sibling
\ goes then.
: CLAIM ( ptr u8 n -- ptr u8 n )
   2dup CLEANUP+ ;

\ Keeps the sibling's path in STAGED and its descriptor in SFD and leaves the
\ output path, so a refusal leaves the stack as it found it.
: RESERVE-STAGED ( ptr u8 n -- ptr u8 n )
   2dup RESERVE-SIBLING SFD ! {: path:ptr size:n :}
   path STAGED size BYTE-COPY
   size STAGED-U ! ;

\ A sibling that cannot be created (a missing or denied directory, an output
\ path too long for the sibling's suffix) is an output that cannot be opened,
\ refused by name like PERSIST's other refusals; nothing exists yet to remove.
: STAGE ( -- )
   OUT-PATH [: RESERVE-STAGED ;] catch {: open-code:n :} 2drop
   open-code 0<> if s" snap: cannot open output" 74 die then
   STAGED-PATH [: CLAIM ;] catch {: claim-code:n :} 2drop
   claim-code 0<> if STAGED-PATH REMOVE-FILE claim-code throw then ;

\ rename replaces the output path in one step: it names the previous file or
\ the whole signed image, never a part of either. An output the rename cannot
\ replace (a directory in its place) is refused by name, and the exit hook
\ removes the sibling.
: PUBLISH ( -- )
   STAGED-PATH CHMOD-X
   STAGED-PATH SIGN-ID:PROG$ CODESIGN:SIGN-AS
   [: STAGED-PATH OUT-PATH RENAME-FILE ;] catch
   0<> if s" snap: cannot replace output" 74 die then ;

: WRITE-PAD ( -- )
   16 0 ?do 0 PAD-ZEROS i + c! loop
   SPAD @ 16 / 0 ?do SFD @ PAD-ZEROS 16 FDIO:WALL loop
   SFD @ PAD-ZEROS SPAD @ 16 mod FDIO:WALL ;

: WRITE-HEAP ( -- )
   SHF @ SNAPSHOT-FORMAT:HEAP-GRID = if SFD @ SGR-PTR SHL @ FDIO:WALL exit then
   SFD @ SND-PTR DATA-START + SHL @ FDIO:WALL ;

: FILL-TRL ( -- )
   SNAP-MAGIC TRL !  SHF @ TRL SNAPSHOT-FORMAT:HEAP-FIELD + !
   ndict@ TRL SNAP-TRL-NDICT + !
   SCL @ TRL SNAP-TRL-REGLEN + !  SDW @ TRL SNAP-TRL-DATALEN + !
   SNAPSHOT-FORMAT:VERSION TRL SNAP-TRL-VERSION + ! ;

: WRITE-BYTES ( -- )
   \ trailer (SNAP-TRL-BYTES): magic, heap form, dict count, region length,
   \ data length, format version - the region stream below is the
   \ canonicalized copy. The version is the LAST field so the magic and the four
   \ older fields sit where the legacy trailer put them, which is what lets the
   \ loader tell a legacy image apart from a corrupt one.
   FILL-TRL
   \ stream: header, engine text, live dict rows, code, structured DATA, trailer
   \ (the heap section last in DATA, raw or grid as SHF says)
   STAGE
   MBUF {: hdr:ptr :}
   SNAP-EXTRA-PTR {: extra:ptr :}
   RESET-BUF
   SFD @ hdr CODE-OFF FDIO:WALL
   SFD @ STB@ STSZ @ FDIO:WALL
   SFD @ SNC-PTR ndict@ DREC * FDIO:WALL
   SFD @ SNC-PTR DICT-SIZE + SCL @ DICT-SIZE - FDIO:WALL
   SFD @ SDL BYTE-VIEW 8 FDIO:WALL
   SFD @ SND-PTR SNAP-RELOC:CALLMAP-OFF FDIO:WALL
   SFD @ SND-PTR SNAP-RELOC:CALLMAP-OFF + DICT-SIZE 32 / + SML @ FDIO:WALL
   SFD @ SND-PTR SNAP-RELOC:ADDRMAP-OFF + DICT-SIZE 32 / + SML @ FDIO:WALL
   SFD @ SND-PTR SNAP-RELOC:XTCELL-N-CELL +
      ADDRESS-CELLS:HEADER-BYTES FDIO:WALL
   SIL @ if SFD @ SND-PTR ADDRESS-CELLS:BOOT-OFF + SIL @ FDIO:WALL then
   WRITE-HEAP
   WRITE-PAD
   SFD @ TRL SNAP-TRL-BYTES FDIO:WALL
   SFD @ extra SNAP-EXTRA-SIZE FDIO:WALL
   SFD @ BEFORE-CLOSE
   SFD @ close-rc 0 <> IF s" snap: output close failed" 74 die THEN ;

: WRITE-IMAGE ( snap -- )
   SNAP-DROP
   WRITE-BYTES ;

defer WRITE-TARGET ( -- )
: WRITE-ARM ( -- ) FRAME WRITE-IMAGE ;
: INSTALL-WRITER ( -- ) [: WRITE-ARM ;] is WRITE-TARGET ;
INSTALL-WRITER

TRUSTED: CF-DEPTH ( -- n ) dbase@ CFSTK-OFF + @ ;

: DATA-SET? ( n -- bool ) data-base + @ 0<> ;

\ Compiler state is zero at rest. Each cell here is set only while a definition
\ compiles, which includes an immediate running inside its body: the
\ control-flow and BEGIN-snapshot depths, the record being built (PEND-CELL),
\ the innermost open quotation (QPATCH-CELL) and the count of enclosing ones
\ parked under it (JIT-QUOT). An image captured with any of them set holds a
\ half-built definition.
: VERIFY-QUIESCENT ( -- )
   CF-DEPTH 0<>
   JIT-SNAP:SP-CELL DATA-SET? or
   PEND-CELL DATA-SET? or
   QPATCH-CELL DATA-SET? or
   JIT-QUOT:SP-CELL DATA-SET? or if
      s" snap: active compiler state at capture" 74 die
   then ;

public

: PATH! ( ptr u8 n -- ) {: path:ptr size:n :}
   size 0 <= size FS-PATH-CAP >= or if
      s" snap: invalid output path length" 74 die
   then
   path OUTPUT size BYTE-COPY
   size OUTPUT-U ! ;

: PERSIST ( -- )
   OUT-PATH-NAMED
   SNAPSHOT-FORMAT:VERIFY
   VERIFY-QUIESCENT
   \ The retained region includes hidden bodies and stored quotations. Checking
   \ only live dictionary records would miss both. Empty code is valid; every
   \ byte of a nonempty retained region needs positive native evidence.
   dbase@ DICT-SIZE + cp@ 2dup <> if
      code-origin 1 <> if
         s" snap: retained code lacks native provenance" ENGINE-ERROR:IMAGE-CODE-ORIGIN die
      then
   else 2drop then
   ADDRESS-CELLS:PERSIST
   GEOMETRY
   CANON-REGION
   CANON-DATA
   ENCODE-HEAP
   WRITE-TARGET
   PUBLISH
   s" " 0 die ;

;package
