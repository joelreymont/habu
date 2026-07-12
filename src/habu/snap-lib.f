\ snap-lib.f — checked snapshot image writer definitions.
\
\ Load after target image emission words (`BUILD-SNAP-HDR`, `SNAP-DROP`,
\ `SNAP-EXTRA-PTR`, `SNAP-EXTRA-SIZE`) and driver I/O. Entry files decide when
\ to prepare checker/include state and call SNAPGO.

\ output path — the single knob; build-fixpoint owns/moves the artifact
: SNAP-OUT s" hb-snap0" TMP-PATH ;

\ Snapshot trailer format version (item 12 slice 3b, dot
\ habu-snapshot-format-ver): once 3b bakes nonzero hidden-field counts into the
\ persisted effect-node arena, a pre-3b engine restoring such an image would
\ misread hidden fields as logical params. The trailer grows 40->48 bytes with
\ a format-version cell at offset 40; the loader (habu2.f EM-SNAPSHOT-RESTORE)
\ rejects any image whose version exceeds what the engine supports with a
\ distinct exit status (80, mirroring the snbad rc-79 corrupt-trailer path).
\ Version 0 (an old 40-byte trailer) still restores. Bump on any layout change
\ that a prior engine would misread.
1 constant SNAP-FORMAT-VERSION
create TRL 48 allot
variable STB  variable STSZ  variable SDB  variable SCL  variable SDL
variable SNL  variable SFTS  variable SPAD  variable SFD
: STB@ STB @ ;
s" STB@" s" -- ptr u8" TRUST
: STB-CELL@ STB @ ;
s" STB-CELL@" s" -- ptr n" TRUST
: SDB@ SDB @ ;
s" SDB@" s" -- ptr u8" TRUST

: SNAP-SIZE! ( -- )
   STSZ @ SCL @ + SDL @ + 48 + SNL ! ;   \ +48: 48-byte format-versioned trailer

: SNAP-HDR! ( -- snap )
   SNL @ BUILD-SNAP-HDR SFTS ! ;

: SNAP-PAD! ( -- snap )
   SNAP-HDR!
   SFTS @ CODE-OFF - SNL @ - SPAD ! ;

: SNAP-STALE ( snap -- )
   SNAP-DROP ;

: SNAP-ABSORB-PAD ( -- snap )
   SNAP-SIZE!
   SNAP-PAD! SNAP-STALE
   SDL @ SPAD @ + SDL !
   SNAP-SIZE!
   SDL @ DATA-SIZE > if s" snap: data payload exceeds image DATA" 74 die then
   SNAP-HDR! ;

: SNAP-RESET-IMAGE-BUFFER ( -- )
   \ MBUF-A is a process-local mmap pointer; restored images must allocate
   \ their own buffer before emitting a fresh ELF/snapshot header.
   0 MBUF-A !
   0 MP !
   0 MLEN! ;

: SNAP-HDR ( -- snap )
   SNAP-RESET-IMAGE-BUFFER
   \ The builder's x20 register constant is XREG-RBASE so it does not shadow
   \ the `rbase` primitive; read the saved text base straight from its cell.
   data-base RBASE-CELL + @ STB !         \ text CONTENT base
   STB-CELL@ CODE-OFF - IMAGE-TEXT-SIZE-OFF + @ IMAGE-TEXT-CONTENT-ADJ - STSZ !  \ own text content size
   dbase@ SDB !
   cp@ SDB @ - SCL !                      \ region payload (dict + compiled code)
   here data-base - SDL !                 \ data payload (through DP)
   SNAP-ABSORB-PAD ;


\ ---- canonical-base persistence ----
\ Snapshot images must be byte-identical across ASLR runs, so the dict/code
\ region is COPIED to scratch and the copy is rebased to canonical base 0
\ with the same engine relocation walks the startup loader uses
\ (snap-rebase primitive -> LSNAPRBD/LSNAPRBC, src/habu/habu2.f). The live
\ region is never touched: rewriting it in place would break the very call
\ chains this writer executes. The trailer records base 0; the loader's
\ delta and membership math are base-agnostic, so restore needs no change.
$1002 constant SNC-MAP-ANON
3 constant SNC-PROT-RW

variable SNC-N

\ Scratch region view: raw anonymous mmap address held as a cell; the
\ typed view is the one audited reinterpret (same class as IMGD-MMAP-PTR).
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
   begin dup DATA-START < while
      dup SND-ZERO-SPAN-CELL
      8 +
   repeat drop ;

: SND-ZERO-LIVE ( -- )
   RBASE-CELL SND-ZERO-CELL   S0-CELL SND-ZERO-CELL
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
   SND-ZERO-EVAL-FRAMES
   SND-ZERO-RSTK ;

: SND-COPY ( -- )
   data-base SND-PTR SDL @ BYTE-COPY ;

\ Quarantined dangling-pointer cells (dot habu-persist-dangling-owners):
\ live process-state cells scattered across persisted structures, each
\ holding an ASLR mmap pointer that is dead after restore (proven by RCA:
\ none is read post-restore; buckets = two instances of the same
\ pre-/post-checker structure classes caching source-text pointers, plus
\ the USIGS snapshot-copy bookkeeping cells). Offsets are tree-dependent;
\ the two-build byte-compare in the snap flow fails closed on any drift,
\ and the owning-field fixes are tracked by the dot above. Keep entries
\ sorted; every change needs the compare green on the exact tree.
create SND-QUARANTINE
   $17CF50 , $17CF68 , $17CF70 , $17D090 , $17D098 ,
   $31D138 , $31D158 , $32EC98 ,
   $51DB60 , $51DB78 , $51DB80 , $51DCA0 ,
   $6CF8A8 , $743ED0 ,
   $757148 , $757150 , $757160 ,
   $75AB80 , $75ABF0 , $75ABF8 ,
20 constant SND-QUARANTINE#

TRUSTED: SND-QUARANTINE@ ( n -- n ) cells SND-QUARANTINE + @ ;

: SND-ZERO-QUARANTINE ( -- )
   SND-QUARANTINE# 0 ?do
      i SND-QUARANTINE@ SND-ZERO-SPAN-CELL
   loop ;

: SNAP-CANON-DATA ( -- )
   SND-ALLOC
   SND-COPY
   SND-ZERO-LIVE
   SND-ZERO-QUARANTINE ;

: SNAP-CANON-REGION ( -- )
   SNC-ALLOC
   SNC-COPY
   SNC-CANON ;

: SNAP-WRITE-BYTES ( -- )
   \ trailer (48 bytes): magic, CANONICAL text base (0), dict count, region
   \ length, data length, format version - the region stream below is the
   \ canonicalized copy. Version at +40 is the last field so the magic/field
   \ offsets 0..40 stay identical to the legacy 40-byte trailer.
   SNAP-MAGIC TRL !  0 TRL 8 + !  ndict@ TRL 16 + !
   SCL @ TRL 24 + !  SDL @ TRL 32 + !  SNAP-FORMAT-VERSION TRL 40 + !
   \ stream: header, engine text, region, data, trailer, zero pad
   SNAP-OUT PATH0 1537 493 open SFD !
   SFD @ 0 < IF s" snap: cannot open output" 74 die THEN
   MBUF {: hdr:ptr :}
   SNAP-EXTRA-PTR {: extra:ptr :}
   SNAP-RESET-IMAGE-BUFFER
   SFD @ hdr CODE-OFF DRV-WALL
   SFD @ STB@ STSZ @ DRV-WALL
   SFD @ SNC-PTR SCL @ DRV-WALL
   SFD @ SND-PTR SDL @ DRV-WALL
   SFD @ TRL 48 DRV-WALL
   SFD @ extra SNAP-EXTRA-SIZE DRV-WALL
   SFD @ close ;

: SNAP-WRITE ( snap -- )
   SNAP-DROP
   SNAP-WRITE-BYTES ;

: SNAPGO ( -- )
   SNAP-HDR
   SNAP-CANON-REGION
   SNAP-CANON-DATA
   SNAP-WRITE
   DRV-EXIT-OK ;

\ Freeze the verify-on-definition hook into the emitted image: hb is fully
\ loaded, so a typed def in its REPL is checked against its sig.
TRUSTED: SNAP-CHECK-HOOK ( ptr u8 n -- n )
   CHECK! dup -1 <> IF 70 throw THEN ;
TRUSTED: SNAP-INSTALL-HOOK ( -- )
   ['] SNAP-CHECK-HOOK set-check ;
