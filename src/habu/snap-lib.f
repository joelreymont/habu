\ snap-lib.f - checked snapshot image writer, package SNAP. Its one public word is SNAP:PERSIST,
\ called by src/habu/app-image-core.f APP-IMAGE:SAVE; it canonicalises both regions, writes and
\ signs a sibling of the output path, renames it over that path and exits.
\ Load after the target image emission words (BUILD-SNAP-HDR, SNAP-DROP, SNAP-EXTRA-PTR,
\ SNAP-EXTRA-SIZE). The core prefix's LOWER-CERT-HOOK survives capture and restore; the writer
\ does not replace it.

require src/lib/fs.f
require src/lib/fs-mutate.f
require src/lib/codesign.f
require src/habu/address-cells.f
require src/habu/snapshot-format.f
require src/habu/cell-grid.f
require src/habu/fdio.f
require src/habu/sign-id.f
require src/habu/stack-abi.f
require src/habu/native-observer-cells.f
require src/habu/native-observer-extension.f
require src/habu/native-body-abi.f
require src/habu/native-body.f
require src/habu/native-host-cells.f

package SNAP

\ The previous-generation host also loads this writer. Its baked SRCLOC, ENGINE-MAIN and layout
\ may predate the owned source cells, ENGINE-MAIN:ARENA-CELL, NDICT-HW-CELL, PROGRAM-FILE-CELL and
\ LOCDEPTHS, so they are restated here; each must equal layout.f's.
$2D68 constant OWNED-SOURCE-CELL
$2D70 constant OWNED-SOURCE-SIZE-CELL
$3868 constant ENGINE-ARENA-CELL
$2D60 constant DICT-HW-CELL
$2DB0 constant PROGRAM-FILE-CELL
$2DC8 constant LOCDEPTHS

create OUTPUT PATH-CAP allot
variable OUTPUT-U

\ Every persist names its output first (PATH!), copied into DATA rather than
\ retained in argv. The image stores none of it (SND-ZERO-WRITER), so a restored
\ process that persists without naming its own path is refused instead of
\ writing where its build did.
: OUT-PATH ( -- ptr u8 n ) OUTPUT OUTPUT-U @ ;

: OUT-PATH-NAMED ( -- )
   OUTPUT-U @ 0= if s" snap: persist has no output path" 74 die then ;

\ src/habu/snapshot-format.f VERSION marks the trailer and DATA layout: an engine restoring another
\ version fails closed (rc 80) instead of misreading it.
create TRL SNAP-TRL-BYTES allot
variable STB  variable STSZ  variable SDB  variable SCL  variable SDL
variable SRL  variable SML  variable SIL  variable SDW
variable SNL  variable SFTS  variable SPAD  variable SFD
variable SHF  variable SHL           \ heap form (SNAPSHOT-FORMAT:HEAP-RAW/-GRID), stored heap bytes
PTR-VARIABLE SXT-A                   \ x86 text scratch (src/habu/snap-x64.f)
create PAD-ZEROS 16 allot
\ Raw views of the snapshot source and buffer cells.
: STB@ STB @ ;
s" STB@" s" -- ptr u8" TRUST
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

\ The running engine's heap floor, where its loader puts the heap back: a host
\ building a tree that moves DATA-START keeps its own.
: HEAP-START ( -- n ) data-base BOOT-LAYOUT:HEAP-START-CELL + @ ;

\ A restored snapshot's authenticated text contains its old region, DATA and trailer: copy only
\ the prefix before them, or each recapture nests the previous snapshot. The loader has validated
\ this trailer; do not guess at older trailers inside the prefix.
: ENGINE-TEXT-SIZE ( -- n )
   data-base TEXT-SIZE-CELL + @ {: bytes:n :}
   data-base SNAP-CELL + CELL-VIEW @ 0= if bytes exit then
   bytes SNAP-TRL-BYTES TEXT-CUT {: end:n :}
   STB@ end + {: trailer:ptr :}
   trailer CELL-VIEW @ SNAP-MAGIC <> if BAD-SOURCE then
   HB-TARGET-LINUX-X86-64? if
      trailer SNAP-TRL-DATALEN + CELL-VIEW @ HEAP-START -
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


\ Images are byte-identical across ASLR runs: the region is copied to scratch and the copy rebased
\ to base 0 with the loader's own walks (snap-rebase -> src/habu/habu2.f LSNAPRBD). Rewriting the
\ live region would break the call chains this writer runs. Each scratch is fresh `map-anon`
\ storage, never unmapped: the writer exits after one image.
PTR-VARIABLE SNC-A

: SNC-PTR ( -- ptr u8 ) SNC-A @ ;

: SNC-ALLOC ( -- )
   SNC-PTR NULL-PTR = 0= if exit then
   SCL @ map-anon 0 <> if drop s" snap: scratch mmap failed" 74 die then
   SNC-A ! ;

: SNC-COPY ( -- )
   SDB@ SNC-PTR SCL @ BYTE-COPY ;

\ snap-rebase takes the copy's bounds as numbers: its distance from zero.
: SNC-CANON ( -- )
   HB-TARGET-LINUX-X86-64? if
      ndict@ DREC * {: used:n :}
      DICT-SIZE used - 0 ?do 0 SNC-PTR used i + + c! loop
      exit
   then
   SNC-PTR NULL-PTR BYTE-VIEW - {: base:n :}
   base  base SCL @ +  ndict@
   STB @  STSZ @  0
   snap-rebase ;


\ Live-state cells in DATA (ASLR text base, stack, argv, cached label addresses) differ per run
\ and are rewritten by habu2.f EM-SNAPSHOT-RESTORE and EM-STARTUP-RUNTIME-STATE; they are zeroed
\ in the copy. tools/hb-build-repl-twin-test.f builds one program twice and compares the images,
\ so a live cell missing here fails that row.
PTR-VARIABLE SND-A

: SND-PTR ( -- ptr u8 ) SND-A @ ;

: SND-ALLOC ( -- )
   SND-PTR NULL-PTR = 0= if exit then
   SDL @ map-anon 0 <> if drop s" snap: data scratch mmap failed" 74 die then
   SND-A ! ;

: SND-ZERO-CELL ( n -- )
   SND-PTR + CELL-VIEW 0 swap ! ;

: SND-ZERO-OFF ( ptr a -- n )
   BYTE-VIEW data-base BYTE-VIEW - ;

\ The running engine places its origin record; an engine without one has no
\ pending take to clear.
: SND-ZERO-PENDING-TAKE ( -- )
   NATIVE-BODY:BODY-CAPABILITY dup NULL-PTR = if drop exit then
   {: record:ptr :}
   record NATIVE-BODY-ABI:PENDING-TAKE cells + SND-ZERO-OFF SND-ZERO-CELL ;

\ FS-ABI and FS-MUT-ABI hold only per-call filesystem scratch: path, stat and descriptor bytes of
\ the build process. Caller-owned buffers and the live task stay.
: SND-ZERO-BAND ( n n -- ) {: off:n bytes:n :}
   bytes 0 ?do off i + SND-ZERO-CELL CELL +loop ;

: SND-ZERO-FS-SCRATCH ( -- )
   FS-MUT-ABI:START FS-MUT-ABI:BYTES SND-ZERO-BAND
   FS-ABI:START FS-ABI:BYTES SND-ZERO-BAND ;

\ STRING-ABI and FMT-ABI hold the SB builder and the integer render buffer: the last text the
\ build process formatted. A restored process starts with an empty builder.
: SND-ZERO-TEXT-SCRATCH ( -- )
   STRING-ABI:START STRING-ABI:BYTES SND-ZERO-BAND
   FMT-ABI:START FMT-ABI:BYTES SND-ZERO-BAND ;

\ MEM-ABI names the build process's open WITH-BYTES scope mappings, which the
\ saved image does not own. Clear it so the image starts with no scope open.
: SND-ZERO-MEM-SCOPE ( -- )
   MEM-ABI:START MEM-ABI:BYTES SND-ZERO-BAND ;

\ BODYLEN bounds the current definition text. A cached FILE replay can leave
\ older source after that bound, while a cold compile writes a different tail.
\ Preserve the live prefix in the copied DATA and retire only its unused room.
: SND-ZERO-BODY-TAIL ( -- )
   BODYLEN-CELL SND-PTR + CELL-VIEW @ {: used:n :}
   used 0 < used BODYBUF-CAP > or if
      s" snap: body length outside buffer" 74 die
   then
   BODYBUF-CAP 2 + used ?do
      0 SND-PTR BODYBUF-OFF i + + c!
   loop ;

\ No definition is open at a capture (VERIFY-QUIESCENT) and every definition's
\ end clears LOCN on either loop (habu2.f C-CLEAR-LOCALS, definers.f
\ DEF-CLOSE), so no local is live. LOCNAMES and LOCDEPTHS still hold the
\ rows of the last compile that declared locals: a cold compile writes them and
\ a definition the cache supplies does not, so both tables clear whole.
: SND-ZERO-LOCALS ( -- )
   LOCNAMES LOC-RECS LOC-REC * SND-ZERO-BAND
   LOCDEPTHS LOC-RECS cells SND-ZERO-BAND ;

\ The return-stack window is a guarded mapping outside DATA: the image holds only its base cell.

: SND-ZERO-LIVE ( -- )
   RBASE-CELL SND-ZERO-CELL   S0-CELL SND-ZERO-CELL
   REGION-ALIAS:DELTA-CELL SND-ZERO-CELL
   STACK-ABI:CAP-CELL SND-ZERO-CELL
   STACK-ABI:REPL-BASE-CELL SND-ZERO-CELL
   STACK-ABI:REPL-CAP-CELL SND-ZERO-CELL
   STACK-ABI:RETURN-BASE-CELL SND-ZERO-CELL
   STACK-ABI:LOOP-BASE-CELL SND-ZERO-CELL
   ARGC-CELL SND-ZERO-CELL    ARGV-CELL SND-ZERO-CELL
   ENVP-CELL SND-ZERO-CELL    SNAP-CELL SND-ZERO-CELL
   PROGRAM-FILE-CELL SND-ZERO-CELL
   HND-CELL SND-ZERO-CELL     PEND-CELL SND-ZERO-CELL
   PKG-PUB-CELL SND-ZERO-CELL PKG-PRI-CELL SND-ZERO-CELL
   PKG-PARENT-CELL SND-ZERO-CELL PKG-REC-CELL SND-ZERO-CELL
   LOOPSP-CELL SND-ZERO-CELL  DOESP-CELL SND-ZERO-CELL
   COMMA-RUN-CELL SND-ZERO-CELL  COMMA-END-CELL SND-ZERO-CELL
   COMMA-BASE-CELL SND-ZERO-CELL
   \ Tier 0 leaves the last closed quotation's entry, EXIT chain and frame in
   \ the Q cells, and each quotation it parked in a JIT-QUOT frame: dead once
   \ closed, and this process's code addresses.
   QENT-CELL SND-ZERO-CELL  QXH-CELL SND-ZERO-CELL  QFRAME-CELL SND-ZERO-CELL
   JIT-QUOT:STK-OFF JIT-QUOT:END JIT-QUOT:STK-OFF - SND-ZERO-BAND
   \ ARM startup reinstalls LCREATE. The x86-64 fixed-address image keeps its
   \ captured Habu CREATE handler, which snapshot startup does not reinstall.
   HB-TARGET-LINUX-X86-64? 0= if CREATEP-CELL SND-ZERO-CELL then
   RRECP-CELL SND-ZERO-CELL
   LMAINP-CELL SND-ZERO-CELL  DOESB-CELL SND-ZERO-CELL  CLAUSE-CELL SND-ZERO-CELL
   EVALREC-CELL SND-ZERO-CELL
   \ The x86-64 handler points into fixed linked code. MAIN installed it before
   \ the sealed image was saved, so snapshot boot keeps that exact XT.
   HB-TARGET-LINUX-X86-64? 0= if UNCGH-CELL SND-ZERO-CELL then
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
   OWNED-SOURCE-CELL SND-ZERO-CELL
   OWNED-SOURCE-SIZE-CELL SND-ZERO-CELL
   SRCLOC:PATH-CELL SND-ZERO-CELL
   SRCLOC:PATHLEN-CELL SND-ZERO-CELL
   SRCLOC:INB-CELL SND-ZERO-CELL
   ENGINE-ARENA-CELL SND-ZERO-CELL
   HIDXP-CELL SND-ZERO-CELL
   HIDX:CLAIMS SND-ZERO-CELL
   ADDRESS-CELLS:INDEX-CELL SND-ZERO-CELL
   TKA-CELL SND-ZERO-CELL     TKL-CELL SND-ZERO-CELL
   PENDTKA-CELL SND-ZERO-CELL
   PEND-OPAQUE-CELL SND-ZERO-CELL
   DEF-TKA-CELL SND-ZERO-CELL
   AOT-SEED-DONE-CELL SND-ZERO-CELL
   BOOT-SRC:USER-END SND-ZERO-CELL
   \ EVALD counts the frames EVAL-TOP links; both belong to the writer's
   \ evaluate (a save from stdin runs inside one), so the image starts with none.
   EVAL-TOP-CELL SND-ZERO-CELL  EVALD-CELL SND-ZERO-CELL
   CLOSED-FREE-CELL SND-ZERO-CELL
   FLOORREC-CELL SND-ZERO-CELL
   \ The release high-water records the writer's rollbacks. The restored DATA
   \ above `here` is fresh, so the image starts with nothing released and its
   \ bytes do not depend on what the build rolled back.
   DPHW-CELL SND-ZERO-CELL
   \ The dictionary high-water counts the records the writer retired. An image
   \ holds only the records below NDICT (SNC-CANON, WRITE-BYTES), so the restored
   \ dictionary has none above it to re-expose.
   DICT-HW-CELL SND-ZERO-CELL
   HB-TARGET-LINUX-X86-64? 0= if CODE-END-CELL SND-ZERO-CELL then
   DEF-OCC:PTR-CELL SND-ZERO-CELL
   NATIVE-OBS-CELLS:HOST-INVALIDATE SND-ZERO-CELL
   NATIVE-OBS-EXT:CAPTURE-INVALIDATE SND-ZERO-CELL
   SND-ZERO-PENDING-TAKE
   NATIVE-HOST-CELLS:SELECT SND-ZERO-CELL
   NATIVE-HOST-CELLS:FACTS SND-ZERO-CELL
   NCOMP-DISPATCH:DEF-TIER-CELL SND-ZERO-CELL
   NCOMP-DISPATCH:BUILD-DEPTH-CELL SND-ZERO-CELL
   NCOMP-DISPATCH:BUILD-TIER-CELL SND-ZERO-CELL
   RSN SND-ZERO-OFF SND-ZERO-CELL
   RSBUF SND-ZERO-OFF {: rs:n :}
   RSBUF-CAP 0 ?do 0 SND-PTR rs i + + c! loop ;

\ Admissions and the seal belong to the writing process. A saved image starts
\ with a fresh vocabulary even when its writer was sealed during capture.
: SND-ZERO-POLICY ( -- )
   POLICY-NDICT-CELL SND-ZERO-CELL
   POLICY-REQUIRE-CELL SND-ZERO-CELL
   PROT-BITS-BYTES 0 ?do POLICY-BITS-OFF i + SND-ZERO-CELL CELL +loop ;

: SND-COPY ( -- )
   data-base SND-PTR SDL @ BYTE-COPY ;

\ The region copy is canonical (region pointers folded to RBASE-VA, call displacements to
\ REGION-OFF), but DATA is copied verbatim, and some of its cells hold region addresses: every
\ deferred word's dispatch cell, the three engine hooks and the compiler dispatch cell. The loader
\ maps the region wherever the kernel puts it, so a
\ stale cell faults on the first deferred call.
\ A cell is never chosen by its contents: `defer`, `is` and cold boot record each DATA offset in
\ the SNAP-RELOC table, which this pass and habu2.f EM-SNAPSHOT-RESTORE both walk.
: SND-XT-CELL@ ( n -- n ) SND-PTR + CELL-VIEW @ ;
: SND-XT-CELL! ( n n -- ) SND-PTR + CELL-VIEW ! ;

: SND-ROWS ( -- ptr n n )
   ADDRESS-CELLS:CURRENT? if SND-PTR SDL @ ADDRESS-CELLS:DATA-SPAN exit then
   SND-PTR SNAP-RELOC:XTCELL-ROWS-OFF + cell-view
   SNAP-RELOC:XTCELL-N-CELL SND-XT-CELL@ ;

: SND-XT-ROW ( n -- n ) {: row:n :}
   SND-ROWS drop row cells + @ ;

: SND-XT-OFF ( n -- n ) SNAP-RELOC:XTCELL-OFF-MASK and ;

: SND-XT-DATA? ( n -- bool ) SNAP-RELOC:XTCELL-DATA-TAG and 0 <> ;

\ The offset indexes the scratch copy, so it must name a whole cell inside DATA, the band the
\ declaration and the loader enforce. The bound is split in two because checked cells are signed:
\ a negative offset fails the first test. Containment only; alignment is not required
\ (src/habu/layout.f).
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

: SND-ZERO-WRITER ( -- )
   SNC-A SND-ZERO-OFF SND-ZERO-CELL
   SND-A SND-ZERO-OFF SND-ZERO-CELL
   SXT-A SND-ZERO-OFF SND-ZERO-CELL
   MBUF-A SND-ZERO-OFF SND-ZERO-CELL
   MP SND-ZERO-OFF SND-ZERO-CELL
   MLEN SND-ZERO-OFF SND-ZERO-CELL
   STB SND-ZERO-OFF SND-ZERO-CELL
   SDB SND-ZERO-OFF SND-ZERO-CELL
   SFD SND-ZERO-OFF SND-ZERO-CELL
   OUTPUT-U SND-ZERO-OFF SND-ZERO-CELL
   OUTPUT SND-ZERO-OFF {: out:n :}
   PATH-CAP 0 ?do out i + SND-ZERO-CELL CELL +loop ;

CAST: PROCESS-CELLS-XT ( n -- [ [ ptr n -- ] -- ] )

: SND-ZERO-PROCESS-CELL ( ptr n -- )
   SND-ZERO-OFF {: off:n :}
   off 0 < off CELL + SDL @ > or if
      s" snap: process cell outside copied data" 74 die
   then
   off SND-ZERO-CELL ;

\ A cell IMAGE-LIFECYCLE:PROCESS-CELL declared holds the running process's
\ value; the image stores it as 0. The visitor is looked up because the
\ previous-generation host loads this writer over a baked registry that has no
\ process cells, and so declares none.
: SND-ZERO-PROCESS ( -- )
   s" IMAGE-LIFECYCLE" -1 search-wl {: wid:n :}
   wid 0= if exit then
   s" PROCESS-CELLS" wid search-wl {: xt:n :}
   xt 0= if exit then
   [: SND-ZERO-PROCESS-CELL ;] xt PROCESS-CELLS-XT execute ;

: CANON-DATA ( -- )
   SND-ALLOC
   DYNAMIC-STORAGE:RELEASE-ALL
   \ Allocation order does not establish liveness: the native string pool
   \ precedes IMK-NDICT0. Owners retire transient state before this copy.
   SND-COPY
   SND-PTR SDL @ CHECKER-REG:RECORD-SNAPSHOT-COPY
   SND-ZERO-LIVE
   SND-ZERO-FS-SCRATCH
   SND-ZERO-TEXT-SCRATCH
   SND-ZERO-MEM-SCOPE
   SND-ZERO-BODY-TAIL
   SND-ZERO-LOCALS
   SND-ZERO-POLICY
   SND-ZERO-WRITER
   SND-ZERO-PROCESS
   HB-TARGET-LINUX-X86-64? 0= if SND-CANON-XT-CELLS then ;

: CANON-REGION ( -- )
   SNC-ALLOC
   SNC-COPY
   SNC-CANON ;

\ The heap, everything above the heap floor, is stored raw or as the smaller
\ cell grid (src/habu/cell-grid.f) of the canonical copy, after CANON-DATA.
variable SBL                         \ flat bitmap bytes, ending at the last present cell's byte
variable SVL                         \ value bytes of the present cells
PTR-VARIABLE SBM-A                   \ flat bitmap scratch
PTR-VARIABLE SGR-A                   \ grid stream scratch

: SBM-PTR ( -- ptr u8 ) SBM-A @ ;
: SGR-PTR ( -- ptr u8 ) SGR-A @ ;

: SCRATCH ( n -- ptr u8 )
   map-anon 0 <> if drop s" snap: heap scratch mmap failed" 74 die then ;

: HEAP-BYTES ( -- n ) SDL @ HEAP-START - ;

: HEAP-CELLS ( -- n )
   HEAP-BYTES CELL-GRID:CELL-BYTES 1- + CELL-GRID:CELL-BYTES / ;

\ The grid rounds the heap up to whole cells; the added bytes lie above the exact extent, read as
\ zero, and the loader refuses a cell that says otherwise.
: HEAP-CELL@ ( n -- n ) {: c:n :}
   HEAP-START c CELL-GRID:CELL-BYTES * + {: off:n :}
   SND-PTR off + CELL-VIEW @ {: v:n :}
   SDL @ off - {: room:n :}
   room CELL-GRID:CELL-BYTES >= if v exit then
   v  1 room 8 * lshift 1-  and ;

: BM-SET ( n -- ) {: c:n :}
   SBM-PTR c CELL-GRID:CELL-BITS / + {: at:ptr :}
   at c@  1 c CELL-GRID:CELL-BITS mod lshift or  at c! ;

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
   SHL @ SCRATCH SGR-A !
   SBM-PTR SBL @ SGR-PTR SNAPSHOT-FORMAT:GRID-FRAME + CELL-GRID:COMPACT {: groups:n stored:n :}
   groups SGR-PTR CELL-VIEW !
   stored SGR-PTR 8 + CELL-VIEW !
   SGR-PTR SNAPSHOT-FORMAT:GRID-FRAME + groups CELL-GRID:PMAP-BYTES + stored + GRID-VALUES ;

: ENCODE-HEAP ( -- )
   SNAPSHOT-FORMAT:HEAP-RAW SHF !  HEAP-BYTES SHL !
   HEAP-CELLS 0= if exit then
   HEAP-CELLS CELL-GRID:CELL-BITS 1- + CELL-GRID:CELL-BITS / SCRATCH SBM-A !
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

\ The image is written to a sibling and renamed over the output path only after the write, close
\ and signature succeed, so a failure never leaves a partial or unsigned executable. The sibling
\ is created exclusively under a per-process name (src/lib/fs-mutate.f FS:RESERVE-SIBLING), so a
\ concurrent writer stages its own; it is chosen after CANON-DATA, so the image never holds its name.
create STAGED PATH-CAP allot
variable STAGED-U

: STAGED-PATH ( -- ptr u8 n ) STAGED STAGED-U @ ;

\ Registered for removal at exit before a byte is written, so every `die` and
\ uncaught throw from here to the rename takes the sibling with it
\ (src/lib/fs-mutate.f CLEANUP-AT-EXIT); after the rename the path names nothing and
\ the removal skips it. A full registry refuses the claim, and the empty sibling
\ goes then.
: CLAIM ( ptr u8 n -- ptr u8 n )
   2dup FS:CLEANUP+ ;

\ Keeps the sibling's path in STAGED and its descriptor in SFD and leaves the
\ output path, so a refusal leaves the stack as it found it.
: RESERVE-STAGED ( ptr u8 n -- ptr u8 n )
   2dup FS:RESERVE-SIBLING SFD ! {: path:ptr size:n :}
   path STAGED size BYTE-COPY
   size STAGED-U ! ;

\ A sibling that cannot be created (a missing or denied directory, an output
\ directory too long for the sibling's path) is an output that cannot be opened,
\ refused by name like PERSIST's other refusals; nothing exists yet to remove.
: STAGE ( -- )
   OUT-PATH [: RESERVE-STAGED ;] catch {: open-code:n :} 2drop
   open-code 0<> if s" snap: cannot open output" 74 die then
   STAGED-PATH [: CLAIM ;] catch {: claim-code:n :} 2drop
   claim-code 0<> if STAGED-PATH FS:REMOVE-FILE claim-code throw then ;

\ rename replaces the output path in one step: it names the previous file or
\ the whole signed image, never a part of either. An output the rename cannot
\ replace (a directory in its place) is refused by name, and the exit hook
\ removes the sibling.
: PUBLISH ( -- )
   STAGED-PATH FS:CHMOD-X
   STAGED-PATH SIGN-ID:PROG$ CODESIGN:SIGN-AS
   [: STAGED-PATH OUT-PATH FS:RENAME-FILE ;] catch
   0<> if s" snap: cannot replace output" 74 die then ;

: WRITE-PAD ( -- )
   16 0 ?do 0 PAD-ZEROS i + c! loop
   SPAD @ 16 / 0 ?do SFD @ PAD-ZEROS 16 FDIO:WALL loop
   SFD @ PAD-ZEROS SPAD @ 16 mod FDIO:WALL ;

: WRITE-HEAP ( -- )
   SHF @ SNAPSHOT-FORMAT:HEAP-GRID = if SFD @ SGR-PTR SHL @ FDIO:WALL exit then
   SFD @ SND-PTR HEAP-START + SHL @ FDIO:WALL ;

: FILL-TRL ( -- )
   SNAP-MAGIC TRL !  SHF @ TRL SNAPSHOT-FORMAT:HEAP-FIELD + !
   ndict@ TRL SNAP-TRL-NDICT + !
   SCL @ TRL SNAP-TRL-REGLEN + !  SDW @ TRL SNAP-TRL-DATALEN + !
   SNAPSHOT-FORMAT:VERSION TRL SNAP-TRL-VERSION + ! ;

: WRITE-BYTES ( -- )
   \ The version is the trailer's last field, so the magic and older fields keep the positions that
   \ let the loader tell an older-format image from a corrupt one.
   FILL-TRL
   \ stream: header, engine text, live dict rows, code, DATA (heap section last, raw or grid as
   \ SHF says), trailer
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

\ The control-flow stack's depth cell lies in the dictionary region, whose
\ base dbase@ answers as a number.
CAST: REGION-CELL ( n -- ptr n )

: CF-DEPTH ( -- n ) dbase@ CFSTK-OFF + REGION-CELL @ ;

: DATA-SET? ( n -- bool ) data-base + @ 0<> ;

\ Compiler state is zero at rest. Each cell here is set only while a definition
\ compiles, which includes an immediate running inside its body: the
\ control-flow and BEGIN-snapshot depths, the record being built (PEND-CELL),
\ the innermost open quotation (QPATCH-CELL) and the count of enclosing ones
\ parked under it (JIT-QUOT). An image captured with any of them set holds a
\ half-built definition. The armed unit guard (UNIT-COMPILE-CELL) sits in the
\ DATA this image copies, and a restored engine would call the dead guard's xt
\ at its first token. An open checker replay never reaches here: a rollback
\ scope's frame makes src/core/checker.f RBF-SNAP-RESET die first (76), and the
\ verifier window closes inside the trusted call that opens it
\ (src/habu/verify-source.f RUN-IN-SCOPE).
: VERIFY-QUIESCENT ( -- )
   CF-DEPTH 0<>
   JIT-SNAP:SP-CELL DATA-SET? or
   PEND-CELL DATA-SET? or
   QPATCH-CELL DATA-SET? or
   JIT-QUOT:SP-CELL DATA-SET? or if
      s" snap: active compiler state at capture" 74 die
   then
   UNIT-COMPILE-CELL DATA-SET? if
      s" snap: active compiler state at capture: a source unit is armed" 74 die
   then ;

public

: PATH! ( ptr u8 n -- ) {: path:ptr size:n :}
   size 0 <= size PATH-CAP >= or if
      s" snap: invalid output path length" 74 die
   then
   path OUTPUT size BYTE-COPY
   size OUTPUT-U ! ;

: PERSIST ( -- )
   OUT-PATH-NAMED
   SNAPSHOT-FORMAT:VERIFY
   VERIFY-QUIESCENT
   ADDRESS-CELLS:PERSIST
   GEOMETRY
   CANON-REGION
   CANON-DATA
   ENCODE-HEAP
   WRITE-TARGET
   PUBLISH
   s" " 0 die ;

;package
