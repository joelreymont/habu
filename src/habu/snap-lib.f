\ snap-lib.f — checked snapshot image writer definitions.
\
\ Load after target image emission words (`BUILD-SNAP-HDR`, `SNAP-DROP`,
\ `SNAP-EXTRA-PTR`, `SNAP-EXTRA-SIZE`) and driver I/O. Entry files decide when
\ to prepare checker/include state and call the writer's `PERSIST`.
\
\ Everything here belongs to package SNAP. The only word an entry file needs is
\ the public `SNAP:PERSIST`; `SNAP:INSTALL-HOOK` is the audited trusted entry
\ that freezes the verify-on-definition hook into an emitted image. The writer
\ state and the scratch-copy machinery stay package-private.
\
\ The entry is `SNAP:PERSIST` - it builds the header, canonicalises the two
\ regions and writes the image, then exits. The tail is deliberately not `GO`:
\ several other files already define a `GO`, the name says nothing about what
\ the word does, and snap.f calls this entry from an UNCHECKED `TRUSTED:` body,
\ where the engine's global-first order would bind a same-named global ahead of
\ the used public with no diagnostic. snap.f imports this package with
\ `using SNAP` and calls the entry by its plain tail.

require lib/fs.f
require lib/codesign.f
require src/habu/address-cells.f
require src/habu/stack-abi.f

package SNAP

create OUTPUT FS-PATH-CAP allot
variable OUTPUT-U

\ Refresh builds keep their existing temporary artifact name. Applications
\ select a path before capture, copied into DATA rather than retained in argv.
: OUT-PATH ( -- ptr u8 n )
   OUTPUT-U @ if OUTPUT OUTPUT-U @ exit then
   s" hb-snap0" TMP-PATH ;

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
variable SNL  variable SFTS  variable SPAD  variable SFD
\ These views expose the raw snapshot source and dictionary/data buffer cells.
\ Retirement: habu-builder-trust-rows-c5d41af6.
: STB@ STB @ ;
s" STB@" s" -- ptr u8" TRUST
: STB-CELL@ STB @ ;
s" STB-CELL@" s" -- ptr n" TRUST
: SDB@ SDB @ ;
s" SDB@" s" -- ptr u8" TRUST

: SIZE! ( -- )
   STSZ @ SCL @ + SDL @ + SNAP-TRL-BYTES + SNL ! ;   \ the format-versioned trailer

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
   SDL @ SPAD @ + SDL !
   SIZE!
   SDL @ DATA-SIZE > if s" snap: data payload exceeds image DATA" 74 die then
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
   end trailer SNAP-TRL-REGLEN + CELL-VIEW @ TEXT-CUT
   trailer SNAP-TRL-DATALEN + CELL-VIEW @ TEXT-CUT ;

: HDR ( -- snap )
   RESET-BUF
   \ The builder's x20 register constant is XREG-RBASE so it does not shadow
   \ the `rbase` primitive; read the saved text base straight from its cell.
   data-base RBASE-CELL + @ STB !         \ text CONTENT base
   ENGINE-TEXT-SIZE STSZ !
   dbase@ SDB !
   cp@ SDB @ - SCL !                      \ region payload (dict + compiled code)
   here data-base - SDL !                 \ data payload (through DP)
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
\ All scratch views, zeroers, and quarantine-table reads retire under
\ habu-builder-trust-rows-c5d41af6.
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
   SNC-N @  SNC-N @ SCL @ +  ndict@
   SNC-TEXT-N  STSZ @  0
   snap-rebase ;


\ Live-state cells inside the persisted DATA region differ per run (ASLR
\ text base, stack, argv, cached label addresses) and are all overwritten
\ by the loader/startup (EM-SNAPSHOT-RESTORE + EM-STARTUP-RUNTIME-STATE,
\ src/habu/habu2.f). Zero them in a scratch copy so images are
\ byte-identical; the two-build compare fails loudly if a new live cell
\ ever appears here without being added.
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
   CREATEP-CELL SND-ZERO-CELL RRECP-CELL SND-ZERO-CELL
   LMAINP-CELL SND-ZERO-CELL  DOESB-CELL SND-ZERO-CELL
   TSIG-A-CELL SND-ZERO-CELL  TSIG-U-CELL SND-ZERO-CELL
   TCSIG-A-CELL SND-ZERO-CELL TCSIG-U-CELL SND-ZERO-CELL
   CRSIG-A-CELL SND-ZERO-CELL CRSIG-U-CELL SND-ZERO-CELL
   INP-CELL SND-ZERO-CELL     INE-CELL SND-ZERO-CELL
   HIDXP-CELL SND-ZERO-CELL
   HIDX:CLAIMS SND-ZERO-CELL
   ADDRESS-CELLS:INDEX-CELL SND-ZERO-CELL
   TKA-CELL SND-ZERO-CELL     TKL-CELL SND-ZERO-CELL
   DEF-TKA-CELL SND-ZERO-CELL
   ENGINE-SNAP-XT-CELL SND-ZERO-CELL
   AOT-SEED-DONE-CELL SND-ZERO-CELL
   BOOT-SRC:USER-END SND-ZERO-CELL
   EVAL-TOP-CELL SND-ZERO-CELL
   NCOMP-DISPATCH:DEF-TIER-CELL SND-ZERO-CELL
   NCOMP-DISPATCH:BUILD-DEPTH-CELL SND-ZERO-CELL
   NCOMP-DISPATCH:BUILD-TIER-CELL SND-ZERO-CELL ;

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

: SND-ZERO-WRITER ( -- )
   SNC-N data-base - SND-ZERO-CELL
   SND-N data-base - SND-ZERO-CELL
   MBUF-A data-base - SND-ZERO-CELL
   MP data-base - SND-ZERO-CELL
   MLEN data-base - SND-ZERO-CELL
   STB data-base - SND-ZERO-CELL
   SDB data-base - SND-ZERO-CELL
   SFD data-base - SND-ZERO-CELL ;

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
   SND-ZERO-WRITER
   SND-CANON-ORIGIN
   SND-CANON-XT-CELLS ;

: CANON-REGION ( -- )
   SNC-ALLOC
   SNC-COPY
   SNC-CANON ;

;package

\ ---- test-only final-close fault seam ----
\ snap-lib.f is builder-only: RETIRE-AND-PERSIST forgets this whole tail before
\ snapshot header is written, so nothing here reaches a shipped image. The seam
\ lets the snapshot-writer suite force the final close to fail and prove
\ the writer's WRITE-BYTES fails closed (rc 74) instead of accepting a
\ half-written image. BEFORE defaults to a no-op; only a test source injected ahead of the
\ snap driver can arm it through INSTALL-TEST, and snap.f undefines that entry on
\ every build so no normal or shipping path can reach it.
package SNAP-CLOSE-SEAM

defer BEFORE ( n -- )

: NOOP ( n -- )
   drop ;

: RESET ( -- )
   [: NOOP ;] is BEFORE ;

RESET

public

: INSTALL-TEST ( [ n -- ] -- )
   is BEFORE ;

: RUN ( n -- )
   BEFORE ;

;package

package SNAP

: WRITE-BYTES ( -- )
   \ trailer (SNAP-TRL-BYTES): magic, CANONICAL text base (0), dict count, region
   \ length, data length, format version - the region stream below is the
   \ canonicalized copy. The version is the LAST field so the magic and the four
   \ older fields sit where the legacy trailer put them, which is what lets the
   \ loader tell a legacy image apart from a corrupt one.
   SNAP-MAGIC TRL !  0 TRL SNAP-TRL-TBASE + !  ndict@ TRL SNAP-TRL-NDICT + !
   SCL @ TRL SNAP-TRL-REGLEN + !  SDL @ TRL SNAP-TRL-DATALEN + !
   ADDRESS-CELLS:SNAPSHOT-FORMAT TRL SNAP-TRL-VERSION + !
   \ stream: header, engine text, region, data, trailer, zero pad
   OUT-PATH PATH0 1537 493 open SFD !
   SFD @ 0 < IF s" snap: cannot open output" 74 die THEN
   MBUF {: hdr:ptr :}
   SNAP-EXTRA-PTR {: extra:ptr :}
   RESET-BUF
   SFD @ hdr CODE-OFF FDIO:WALL
   SFD @ STB@ STSZ @ FDIO:WALL
   SFD @ SNC-PTR SCL @ FDIO:WALL
   SFD @ SND-PTR SDL @ FDIO:WALL
   SFD @ TRL 48 FDIO:WALL
   SFD @ extra SNAP-EXTRA-SIZE FDIO:WALL
   SFD @ SNAP-CLOSE-SEAM:RUN
   SFD @ close-rc 0 <> IF s" snap: output close failed" 74 die THEN ;

: WRITE-IMAGE ( snap -- )
   SNAP-DROP
   WRITE-BYTES ;

\ Freeze the verify-on-definition hook into the emitted image: hb is fully
\ loaded, so a typed def in its REPL is checked against its sig.
\ Retirement: CHECK-HOOK under cap:checker-hook-identity;
\ INSTALL-HOOK under habu-builder-trust-rows-c5d41af6.
TRUSTED: CHECK-HOOK ( ptr u8 n -- n )
   CHECK! dup -1 <> IF 70 throw THEN ;

public

: PATH! ( ptr u8 n -- ) {: path:ptr size:n :}
   size 0 <= size FS-PATH-CAP >= or if
      s" snap: invalid output path length" 74 die
   then
   path OUTPUT size BYTE-COPY
   size OUTPUT-U ! ;

: PERSIST ( -- )
   \ The retained region includes hidden bodies and stored quotations. Checking
   \ only live dictionary records would miss both. Empty code is valid; every
   \ byte of a nonempty retained region needs positive native evidence.
   dbase@ DICT-SIZE + cp@ 2dup <> if
      code-origin 1 <> if
         s" snap: retained code lacks native provenance" ENGINE-ERROR:IMAGE-CODE-ORIGIN die
      then
   else 2drop then
   ADDRESS-CELLS:PERSIST
   HDR
   CANON-REGION
   CANON-DATA
   WRITE-IMAGE
   OUT-PATH CODESIGN:ENSURE
   DRV-EXIT-OK ;

TRUSTED: INSTALL-HOOK ( -- )
   LOWER-CERT-HOOK:INSTALL
   ['] CHECK-HOOK set-check ;

;package
