\ snap-lib.f — checked snapshot image writer definitions.
\
\ Load after target image emission words (`BUILD-SNAP-HDR`, `SNAP-DROP`,
\ `SNAP-EXTRA-PTR`, `SNAP-EXTRA-SIZE`) and driver I/O. Entry files decide when
\ to prepare checker/include state and call the writer's `SNAPGO`.
\
\ Everything here belongs to package SNAP. The only word an entry file needs is
\ the public `SNAP:SNAPGO`; `SNAP:INSTALL-HOOK` is the audited trusted entry
\ that freezes the verify-on-definition hook into an emitted image. The writer
\ state and the scratch-copy machinery stay package-private.
\
\ The public entry keeps its historic spelling `SNAPGO` rather than a shorter
\ tail: src/habu/snap.f still defines global words, so renaming the call site
\ would make its driver a changed global definition and pull that file (and the
\ build-fixpoint test that pins its emitted text) into this change. Those files
\ get their own package owners under a separate dot; snap.f imports this
\ package with `using SNAP` in the meantime.

package SNAP

\ output path — the single knob; build-fixpoint owns/moves the artifact
: OUT-PATH ( -- ptr u8 n )
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
   0 MBUF-A !
   0 MP !
   0 MLEN! ;

: HDR ( -- snap )
   RESET-BUF
   \ The builder's x20 register constant is XREG-RBASE so it does not shadow
   \ the `rbase` primitive; read the saved text base straight from its cell.
   data-base RBASE-CELL + @ STB !         \ text CONTENT base
   STB-CELL@ CODE-OFF - IMAGE-TEXT-SIZE-OFF + @ IMAGE-TEXT-CONTENT-ADJ - STSZ !  \ own text content size
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

\ Zero a whole data-relative span in the scratch copy (evaluate frames are
\ startup-transient input state; every cell is dead after restore).
TRUSTED: SND-ZERO-SPAN-CELL ( n -- ) SND-N @ + 0 swap ! ;

: SND-ZERO-EVAL-FRAMES ( -- )
   EVAL-FRAME
   begin dup EVAL-FRAME EVAL-MAX-DEPTH EVAL-FRAME-SIZE * + < while
      dup SND-ZERO-SPAN-CELL
      8 +
   repeat drop ;

\ The return-stack window is transient machine state; stale slots can hold
\ dangling arena pointers from the build (proven: two old-USIGS pointers
\ survived here). Dead after restore - zero the whole window.
: SND-ZERO-RSTK ( -- )
   RSTK-OFF
   begin dup RSTK-END < while
      dup SND-ZERO-SPAN-CELL
      8 +
   repeat drop ;

: SND-ZERO-LIVE ( -- )
   RBASE-CELL SND-ZERO-CELL   S0-CELL SND-ZERO-CELL
   REPLH-CELL SND-ZERO-CELL
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
   TKA-CELL SND-ZERO-CELL     TKL-CELL SND-ZERO-CELL
   DEF-TKA-CELL SND-ZERO-CELL
   ENGINE-SNAP-XT-CELL SND-ZERO-CELL
   \ Disarm the top-row token hook in the image: a captured-armed tracker fires
   \ on the warm boot's `provided` re-establishment rows and its sig-store scan
   \ derefs an un-rebased engine-text pointer (SIGSEGV; dot
   \ habu-typed-top-snapshot-daa8989a owns tracker-on re-arm; the pointer defect
   \ is habu-snapshot-rebase-persisted-4bd33351).
   TOP-HOOK-CELL SND-ZERO-CELL
   AOT-SEED-DONE-CELL SND-ZERO-CELL
   AOT-SEED-ARM-CELL SND-ZERO-CELL
   SND-ZERO-EVAL-FRAMES
   SND-ZERO-RSTK ;

: SND-COPY ( -- )
   data-base SND-PTR SDL @ BYTE-COPY ;

\ ---- the heap the refresh prelude abandoned ---------------------------------
\ The native refresh truncates the dictionary back to the primitive boundary
\ (src/habu/hide.f BFR-HIDE-DICT-FROM-EARLIEST, driven by tools/build-fixpoint.f
\ BF-STAGE2-HIDE-DEFS) and then reloads the whole prefix from source. Truncating
\ the dictionary does not move DP, so everything the previous generation had
\ allotted stays in the DP heap with no owner and no reader -- and it still holds
\ that generation's own mmap addresses and region pointers, which the image then
\ carries into runs where they mean nothing. Measured on this tree with
\ tools/snap-heap-owner.f: 4.48 MB of abandoned heap holding 50 of the 113 heap cells
\ that differ between two builds of the same image.
\ The live generation starts at IMK-NDICT0, the first variable of the first
\ prefix source file (src/core/util.f records the primitive record watermark
\ there precisely because it is first), so everything below it is abandoned. A
\ build with no truncation ahead of it puts IMK-NDICT0 at DATA-START and the
\ span is empty, which is the same rule with nothing to do.
\ This replaces a table of twenty hardcoded offsets that had gone stale: measured
\ against the actual two-build difference, eight of them pointed inside this same
\ abandoned heap and the other twelve zeroed cells in live checker buffers that
\ do not differ between builds at all.
\ IMK-NDICT0 is a prefix-internal word: the whole engine prefix loads inside the
\ refresh prelude's check-off window, so it carries no charted effect and checked
\ code cannot name it. This is the same named trusted boundary src/habu/snap.f
\ uses to reach CHECKER-SNAPSHOT-PREPARE, and for the same reason.
TRUSTED: SND-DEAD-HEAP-END ( -- n )
   IMK-NDICT0 data-base - ;

: SND-ZERO-DEAD-HEAP ( -- )
   SND-DEAD-HEAP-END {: end:n :}
   end DATA-START < if
      s" snap: live heap starts below DATA-START" 74 die
   then
   DATA-START
   begin dup 8 + end <= while
      dup SND-ZERO-SPAN-CELL
      8 +
   repeat drop ;

\ ---- persisted cells that hold a JIT-region address --------------------------
\ Everything inside the region copy is already canonicalised: pointers into the
\ region are folded to the RBASE-VA sentinel and call displacements to the
\ canonical REGION-OFF distance. Some cells in DATA hold region addresses too --
\ every deferred word's dispatch cell, and the three engine hook cells -- and DATA
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
\ the three hook cells -- and records the DATA offset in the table this pass
\ walks. The loader (habu2.f EM-SNAPSHOT-RESTORE) inverts exactly this list from
\ exactly the same table.
\ These four words belong to this package, the snapshot writer, rather than to
\ SNAP-RELOC: the engine owns the declaring and the restoring, and the writer owns
\ the one pass that runs over its own scratch copy. They read the table's shape
\ from SNAP-RELOC and nothing else.
TRUSTED: SND-XT-CELL@ ( n -- n ) SND-N @ + @ ;
TRUSTED: SND-XT-CELL! ( n n -- ) SND-N @ + ! ;

: SND-XT-ROW ( n -- n ) {: row:n :}
   SNAP-RELOC:XTCELL-ROWS-OFF row cells + SND-XT-CELL@ ;

: SND-CANON-XT-CELL ( n -- ) {: cell:n :}
   cell SND-XT-CELL@ {: xt:n :}
   xt 0= if exit then
   xt dbase@ - RBASE-VA +  cell SND-XT-CELL! ;

: SND-CANON-XT-CELLS ( -- )
   SNAP-RELOC:XTCELL-N-CELL SND-XT-CELL@ 0 ?do
      i SND-XT-ROW SND-CANON-XT-CELL
   loop ;

: CANON-DATA ( -- )
   SND-ALLOC
   SND-COPY
   SND-ZERO-LIVE
   SND-ZERO-DEAD-HEAP
   SND-CANON-XT-CELLS ;

: CANON-REGION ( -- )
   SNC-ALLOC
   SNC-COPY
   SNC-CANON ;

;package

\ ---- test-only final-close fault seam ----
\ snap-lib.f is builder-only: SNAP-RETIRE-GO forgets this whole tail before the
\ snapshot header is written, so nothing here reaches a shipped image. The seam
\ lets the snapshot-writer suite force the final close to fail and prove
\ the writer's WRITE-BYTES fails closed (rc 74) instead of accepting a
\ half-written image. BEFORE defaults to a no-op; only a test source injected ahead of the
\ snap driver can arm it, and snap.f undefines the target on
\ every build so no normal or shipping path can reach it.
package SNAP-CLOSE-SEAM

public
defer BEFORE ( n -- )

private

: NOOP ( n -- )
   drop ;

: RESET ( -- )
   [: NOOP ;] is BEFORE ;

RESET

public

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
   SNAP-FORMAT-VERSION TRL SNAP-TRL-VERSION + !
   \ stream: header, engine text, region, data, trailer, zero pad
   OUT-PATH PATH0 1537 493 open SFD !
   SFD @ 0 < IF s" snap: cannot open output" 74 die THEN
   MBUF {: hdr:ptr :}
   SNAP-EXTRA-PTR {: extra:ptr :}
   RESET-BUF
   SFD @ hdr CODE-OFF DRV-WALL
   SFD @ STB@ STSZ @ DRV-WALL
   SFD @ SNC-PTR SCL @ DRV-WALL
   SFD @ SND-PTR SDL @ DRV-WALL
   SFD @ TRL 48 DRV-WALL
   SFD @ extra SNAP-EXTRA-SIZE DRV-WALL
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

: SNAPGO ( -- )
   HDR
   CANON-REGION
   CANON-DATA
   WRITE-IMAGE
   DRV-EXIT-OK ;

TRUSTED: INSTALL-HOOK ( -- )
   LOWER-CERT-HOOK:INSTALL
   ['] CHECK-HOOK set-check ;

;package
