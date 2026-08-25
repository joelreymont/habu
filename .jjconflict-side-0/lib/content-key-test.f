\ content-key-test.f - focused tests for content-key digest caching.
\ Run: bin/hb --load lib/errors.f lib/string.f lib/test.f lib/memory.f lib/fs.f lib/fs-mutate.f lib/content-key.f lib/content-key-test.f

require lib/errors.f
require lib/string.f
require lib/test.f
require lib/memory.f
require lib/fs.f
require lib/fs-mutate.f
require lib/content-key.f

\ White-box test: reopen the module's package so the fixtures reach content-key's
\ private capacity/length cells (CK-CACHE-CAP, CK-HEX-LEN), the persist fault seam
\ (CK-FAULT-ARM, CK-CLEAN-FAULT, CK-CLEAN-N) and the dirty flag (CK-CACHE-DIRTY),
\ and call the public builders by their bare package-local names.
package CONTENT-KEY

$8000 constant CKT-READ-CAP
64 constant CKT-KEY-LEN
$8000 constant CKT-BUILD-CAP
CK-CACHE-CAP 4096 + constant CKT-BIG-CAP

\ Distinctive throw codes for the persist fault seam: positive test sentinels that
\ cannot collide with a real (negative) filesystem error code, so an assertion on
\ the exact code proves it is the injected one that surfaced. A and B differ so a
\ swallowed cleanup failure (B) can be told apart from the primary error (A).
$7F01 constant CKT-FAULT-A
$7F02 constant CKT-FAULT-B

variable CKT-ROOT-U
variable CKT-SRC-U
variable CKT-CACHE-U
variable CKT-BUILD-U
variable CKT-BIG-A
variable CKT-TMP-COUNT
variable CKT-OLD-U
variable CKT-BADCACHE-U

create CKT-ROOT FS-PATH-CAP allot
create CKT-SRC FS-PATH-CAP allot
create CKT-CACHE FS-PATH-CAP allot
create CKT-KEY1 80 allot
create CKT-KEY2 80 allot
create CKT-SEQ-A 80 allot
create CKT-SEQ-B 80 allot
create CKT-INT-A 80 allot
create CKT-INT-B 80 allot
create CKT-READ CKT-READ-CAP allot
create CKT-DG 40 allot
create CKT-DGHEX 80 allot
create CKT-BUILD CKT-BUILD-CAP allot
create CKT-OLD CKT-READ-CAP allot
create CKT-BADCACHE FS-PATH-CAP allot

: CKT-COPY! ( ptr u8 n ptr u8 ptr n -- ) {: a:ptr u:n dst:ptr lenp:ptr :}
   a dst u BYTE-COPY
   u lenp ! ;

: CKT-PATH! ( ptr u8 n ptr u8 n ptr u8 ptr n -- )
   {: pa:ptr pu:n na:ptr nu:n dst:ptr lenp:ptr :}
   pa pu na nu dst JOIN-PATH lenp ! ;

: CKT-ROOT$ ( -- ptr u8 n )
   CKT-ROOT CKT-ROOT-U @ ;

: CKT-SRC$ ( -- ptr u8 n )
   CKT-SRC CKT-SRC-U @ ;

: CKT-CACHE$ ( -- ptr u8 n )
   CKT-CACHE CKT-CACHE-U @ ;

: CKT-SETUP ( -- )
   CLEANUP-RESET
   s" habu-content-key" TMPDIR-MKDIR CKT-ROOT CKT-ROOT-U CKT-COPY!
   CKT-ROOT$ CLEANUP-TREE+
   CKT-ROOT$ s" src.f" CKT-SRC CKT-SRC-U CKT-PATH!
   CKT-ROOT$ s" content-key.cache" CKT-CACHE CKT-CACHE-U CKT-PATH!
   CACHE-CLEAR!
   CKT-CACHE$ CACHE-PATH! ;

: CKT-KEY! ( ptr u8 -- ) {: dst:ptr :}
   OPEN
   s" content-key-test" TEXT+
   CKT-SRC$ FILE+
   dst FINAL-HEX ;

: CKT-LF-COUNT ( ptr u8 n -- n ) {: a:ptr u:n :}
   0 0 begin dup u < while
      dup a + c@ STR-LF = if swap 1+ swap then
      1+
   repeat drop ;

: CKT-CACHE-ROWS ( -- n )
   CKT-CACHE$ CKT-READ CKT-READ-CAP READ-ALL
   CKT-READ swap CKT-LF-COUNT ;

\ Force the next cache touch to reload from disk under the current path.
: CKT-REPOINT ( -- )
   CACHE-CLEAR!
   CKT-CACHE$ CACHE-PATH! ;

\ ---- raw on-disk row builder (fabricates the unchanged cache format) ----------

: CKT-BUILD-RESET ( -- )
   0 CKT-BUILD-U ! ;

: CKT-BUILD-C+ ( n -- ) {: c:n :}
   CKT-BUILD-U @ CKT-BUILD-CAP >= if E-STR-CAPACITY throw then
   c CKT-BUILD CKT-BUILD-U @ + c!
   CKT-BUILD-U @ 1+ CKT-BUILD-U ! ;

: CKT-BUILD+ ( ptr u8 n -- ) {: a:ptr u:n :}
   CKT-BUILD-U @ u + CKT-BUILD-CAP > if E-STR-CAPACITY throw then
   a CKT-BUILD CKT-BUILD-U @ + u BYTE-COPY
   CKT-BUILD-U @ u + CKT-BUILD-U ! ;

: CKT-BUILD-N+ ( n -- ) {: n:n :}
   n 0 < if E-STR-BOUNDS throw then
   n 10 >= if n 10 / RECURSE then
   n 10 mod STR-ZERO + CKT-BUILD-C+ ;

: CKT-HEX0+ ( -- )
   64 0 DO STR-ZERO CKT-BUILD-C+ LOOP ;

: CKT-DIGEST! ( -- )
   CKT-SRC$ CKT-DG SHA256-FILE dup 0 <> if throw then drop
   CKT-DG CKT-DGHEX SHA256>HEX ;

\ The exact current row for src.f: real size/mtime/ctime and the real digest.
: CKT-CORRECT-ROW+ ( -- )
   CKT-DIGEST!
   CKT-SRC$ FILE-META {: sz:n mt:n mn:n ct:n cn:n :}
   CKT-SRC$ CKT-BUILD+
   STR-TAB CKT-BUILD-C+ sz CKT-BUILD-N+
   STR-TAB CKT-BUILD-C+ mt CKT-BUILD-N+
   STR-TAB CKT-BUILD-C+ mn CKT-BUILD-N+
   STR-TAB CKT-BUILD-C+ ct CKT-BUILD-N+
   STR-TAB CKT-BUILD-C+ cn CKT-BUILD-N+
   STR-TAB CKT-BUILD-C+ CKT-DGHEX CK-HEX-LEN CKT-BUILD+
   STR-LF CKT-BUILD-C+ ;

\ A stale row for src.f: same path, non-matching metadata, placeholder digest.
: CKT-STALE-ROW+ ( n -- ) {: i:n :}
   CKT-SRC$ CKT-BUILD+
   STR-TAB CKT-BUILD-C+ 999 CKT-BUILD-N+
   STR-TAB CKT-BUILD-C+ i CKT-BUILD-N+
   STR-TAB CKT-BUILD-C+ 1 CKT-BUILD-N+
   STR-TAB CKT-BUILD-C+ 1 CKT-BUILD-N+
   STR-TAB CKT-BUILD-C+ 1 CKT-BUILD-N+
   STR-TAB CKT-BUILD-C+ CKT-HEX0+
   STR-LF CKT-BUILD-C+ ;

\ A row for an unrelated path, to prove multi-row old-format files still read.
: CKT-OTHER-ROW+ ( -- )
   s" other.f" CKT-BUILD+
   STR-TAB CKT-BUILD-C+ 10 CKT-BUILD-N+
   STR-TAB CKT-BUILD-C+ 2 CKT-BUILD-N+
   STR-TAB CKT-BUILD-C+ 3 CKT-BUILD-N+
   STR-TAB CKT-BUILD-C+ 4 CKT-BUILD-N+
   STR-TAB CKT-BUILD-C+ 5 CKT-BUILD-N+
   STR-TAB CKT-BUILD-C+ CKT-HEX0+
   STR-LF CKT-BUILD-C+ ;

: CKT-WRITE-BUILD ( -- )
   CKT-CACHE$ CKT-BUILD CKT-BUILD-U @ WRITE-ALL ;

: CKT-BIG ( -- ptr u8 )
   CKT-BIG-A 0 ptr-field @ 0= if
      CKT-BIG-CAP MEM:BYTES-ALLOC-LEN MEM:ALLOC-BYTES drop CKT-BIG-A 0 ptr-field !
   then
   CKT-BIG-A 0 ptr-field @ ;

\ ---- existing behaviour ------------------------------------------------------

: CKT-CACHE-STABLE-HIT ( -- )
   CKT-SRC$ s" alpha" WRITE-ALL
   CKT-KEY1 CKT-KEY!
   CKT-CACHE$ FILE? TTRUE
   CKT-CACHE-ROWS 1 T=
   CKT-KEY2 CKT-KEY!
   CKT-KEY2 CKT-KEY-LEN CKT-KEY1 CKT-KEY-LEN T$=
   CKT-CACHE-ROWS 1 T= ;

\ Editing src.f changes the key; the appended row and the stale row collapse to
\ one on save (newest-per-path compaction), so the file does not grow.
: CKT-CACHE-INVALIDATES ( -- )
   CKT-SRC$ s" beta-more" WRITE-ALL
   CKT-KEY2 CKT-KEY!
   CKT-KEY2 CKT-KEY-LEN CKT-KEY1 CKT-KEY-LEN T$<>
   CKT-CACHE-ROWS 1 T= ;

\ ---- compaction, backward read, cap behaviour, atomic write ------------------

: CKT-CACHE-BLOATED-COMPACTS ( -- )
   CKT-SRC$ s" gamma-content" WRITE-ALL
   CKT-KEY1 CKT-KEY!
   CKT-BUILD-RESET
   50 0 DO i CKT-STALE-ROW+ LOOP
   CKT-CORRECT-ROW+
   CKT-WRITE-BUILD
   CKT-REPOINT
   CKT-CACHE-ROWS 51 T=
   CKT-KEY2 CKT-KEY!
   CKT-KEY2 CKT-KEY-LEN CKT-KEY1 CKT-KEY-LEN T$=
   CKT-CACHE-ROWS 1 T= ;

: CKT-CACHE-BACKCOMPAT ( -- )
   CKT-SRC$ s" delta-payload" WRITE-ALL
   CKT-KEY1 CKT-KEY!
   CKT-BUILD-RESET
   CKT-OTHER-ROW+
   CKT-CORRECT-ROW+
   CKT-WRITE-BUILD
   CKT-REPOINT
   CKT-KEY2 CKT-KEY!
   CKT-KEY2 CKT-KEY-LEN CKT-KEY1 CKT-KEY-LEN T$= ;

\ Over-cap cache: announced on stderr (not captured by this harness) and rebuilt
\ compacted rather than abandoned. We assert the post-condition: the file is back
\ under the cap and lookups work.
: CKT-CACHE-OVERCAP-REBUILDS ( -- )
   CKT-SRC$ s" epsilon" WRITE-ALL
   CKT-CACHE$ CKT-BIG CKT-BIG-CAP WRITE-ALL
   CKT-REPOINT
   CKT-KEY1 CKT-KEY!
   CKT-CACHE$ FILE-SIZE CK-CACHE-CAP > 0= TTRUE
   CKT-CACHE-ROWS 1 T=
   CKT-KEY2 CKT-KEY!
   CKT-KEY2 CKT-KEY-LEN CKT-KEY1 CKT-KEY-LEN T$= ;

: CKT-ENDS-TMP? ( ptr u8 n -- bool ) {: a:ptr u:n :}
   u 4 < if CK-FALSE exit then
   a u 4 - + 4 s" .tmp" STR= ;

: CKT-COUNT-TMP ( ptr u8 n -- )
   BASENAME CKT-ENDS-TMP? if CKT-TMP-COUNT @ 1+ CKT-TMP-COUNT ! then ;

: CKT-CACHE-ATOMIC-POSTCOND ( -- )
   CKT-SRC$ s" zeta" WRITE-ALL
   CKT-REPOINT
   CKT-KEY1 CKT-KEY!
   0 CKT-TMP-COUNT !
   CKT-ROOT$ [: CKT-COUNT-TMP ;] WALK-FILES
   CKT-TMP-COUNT @ 0 T=
   CKT-REPOINT
   CKT-KEY2 CKT-KEY!
   CKT-KEY2 CKT-KEY-LEN CKT-KEY1 CKT-KEY-LEN T$=
   CKT-CACHE-ROWS 1 T= ;

\ ---- persist failure propagation ---------------------------------------------
\ These prove the atomic writer never reports success for a failed save: the
\ original throw code reaches the caller, the loaded cache and its dirty flag are
\ untouched, no partial/new file replaces the old one, cleanup runs exactly once,
\ a cleanup failure cannot mask the primary error, and a later retry persists.
\ Real filesystem states drive one case (a write into a missing directory); the
\ others arm content-key's fault seam because a real filesystem cannot produce
\ the needed combinations (a temp written yet forced to fail removal, or a rename
\ that fails over an intact file).

: CKT-OLD$ ( -- ptr u8 n )
   CKT-OLD CKT-OLD-U @ ;

: CKT-BADCACHE$ ( -- ptr u8 n )
   CKT-BADCACHE CKT-BADCACHE-U @ ;

\ Snapshot the on-disk cache bytes for later "old file unchanged" comparisons.
: CKT-SNAPSHOT ( -- )
   CKT-CACHE$ CKT-OLD CKT-READ-CAP READ-ALL CKT-OLD-U ! ;

\ Current on-disk cache bytes.
: CKT-CUR$ ( -- ptr u8 n )
   CKT-CACHE$ CKT-READ CKT-READ-CAP READ-ALL {: got:n :}
   CKT-READ got ;

: CKT-TMP-LEFT ( -- n )
   0 CKT-TMP-COUNT !
   CKT-ROOT$ [: CKT-COUNT-TMP ;] WALK-FILES
   CKT-TMP-COUNT @ ;

: CKT-RM-TMP-ONE ( ptr u8 n -- ) {: a:ptr u:n :}
   a u BASENAME CKT-ENDS-TMP? if a u REMOVE-FILE then ;

: CKT-CLEAN-TMPS ( -- )
   CKT-ROOT$ [: CKT-RM-TMP-ONE ;] WALK-FILES ;

\ Fresh state for a persist-failure case: no stale temps, no old cache file, then
\ one successful save (the "old" file we snapshot) and an edit to src so the next
\ key build appends a new row and marks the cache dirty.
: CKT-PERSIST-PREP ( -- )
   CKT-CLEAN-TMPS
   CKT-CACHE$ EXISTS? if CKT-CACHE$ REMOVE-FILE then
   CKT-REPOINT
   CK-FAULT-RESET
   CKT-SRC$ s" persist-base" WRITE-ALL
   CKT-KEY1 CKT-KEY!
   CKT-SNAPSHOT
   CKT-SRC$ s" persist-edited" WRITE-ALL ;

\ Building the unique temp fails: nothing is written, so the old file and dirty
\ flag are intact and cleanup runs once (finding no temp).
: CKT-PERSIST-TEMP-FAULT ( -- )
   CKT-PERSIST-PREP
   CKT-FAULT-A CK-FAULT-TEMP CK-FAULT-ARM
   [: CKT-KEY2 CKT-KEY! ;] CKT-FAULT-A TTHROWSQ
   CK-CACHE-DIRTY @ 0 T<>
   CKT-CUR$ CKT-OLD$ T$=
   CK-CLEAN-N @ 1 T=
   CKT-TMP-LEFT 0 T= ;

\ The write step fails before any temp is created: old file and dirty flag intact.
: CKT-PERSIST-WRITE-FAULT ( -- )
   CKT-PERSIST-PREP
   CKT-FAULT-A CK-FAULT-WRITE CK-FAULT-ARM
   [: CKT-KEY2 CKT-KEY! ;] CKT-FAULT-A TTHROWSQ
   CK-CACHE-DIRTY @ 0 T<>
   CKT-CUR$ CKT-OLD$ T$=
   CK-CLEAN-N @ 1 T=
   CKT-TMP-LEFT 0 T= ;

\ The rename fails after the temp is written: the old file is never replaced and
\ the temp is cleaned up exactly once, so no partial cache survives.
: CKT-PERSIST-RENAME-FAULT ( -- )
   CKT-PERSIST-PREP
   CKT-FAULT-A CK-FAULT-RENAME CK-FAULT-ARM
   [: CKT-KEY2 CKT-KEY! ;] CKT-FAULT-A TTHROWSQ
   CK-CACHE-DIRTY @ 0 T<>
   CKT-CUR$ CKT-OLD$ T$=
   CK-CLEAN-N @ 1 T=
   CKT-TMP-LEFT 0 T= ;

\ Rename fails (code A) AND the cleanup removal fails (code B, swallowed): the
\ caller must still see A, proving cleanup failure never masks the primary error.
\ The temp is left behind because its removal failed.
: CKT-PERSIST-NOMASK ( -- )
   CKT-PERSIST-PREP
   CKT-FAULT-A CK-FAULT-RENAME CK-FAULT-ARM
   CKT-FAULT-B CK-CLEAN-FAULT !
   [: CKT-KEY2 CKT-KEY! ;] CKT-FAULT-A TTHROWSQ
   CK-CACHE-DIRTY @ 0 T<>
   CKT-CUR$ CKT-OLD$ T$=
   CK-CLEAN-N @ 1 T=
   CKT-TMP-LEFT 1 T= ;

\ After a failed save, clearing the fault and retrying persists cleanly: dirty is
\ cleared, the file is compacted to one row, and it now differs from the old one.
: CKT-PERSIST-RETRY ( -- )
   CKT-PERSIST-PREP
   CKT-FAULT-A CK-FAULT-RENAME CK-FAULT-ARM
   [: CKT-KEY2 CKT-KEY! ;] CKT-FAULT-A TTHROWSQ
   CK-CACHE-DIRTY @ 0 T<>
   CK-FAULT-RESET
   CKT-KEY2 CKT-KEY!
   CK-CACHE-DIRTY @ 0 T=
   CKT-CACHE-ROWS 1 T=
   CKT-CUR$ CKT-OLD$ T$<>
   CKT-TMP-LEFT 0 T= ;

\ A genuine filesystem write failure (target directory absent) throws E-FS-OPEN
\ and that real error identity reaches the caller; dirty stays set, no file made.
: CKT-PERSIST-REAL-MISSING-DIR ( -- )
   CKT-CLEAN-TMPS
   CACHE-CLEAR!
   CKT-ROOT$ s" nodir/content-key.cache" CKT-BADCACHE CKT-BADCACHE-U CKT-PATH!
   CKT-BADCACHE$ CACHE-PATH!
   CK-FAULT-RESET
   CKT-SRC$ s" real-miss" WRITE-ALL
   [: CKT-KEY1 CKT-KEY! ;] E-FS-OPEN TTHROWSQ
   CK-CACHE-DIRTY @ 0 T<>
   CKT-BADCACHE$ EXISTS? TFALSE
   CKT-REPOINT ;

: CKT-CLEANUP ( -- )
   CACHE-CLEAR!
   CLEANUP-RUN
   CKT-ROOT$ EXISTS? TFALSE ;

\ ---- overlapping folds ------------------------------------------------------
\ The regression this module's fold handles exist for. Two folds run one after
\ the other, then the SAME two folds run overlapping - the second opened while
\ the first is still being folded into, which is what a key derived inside
\ another key's derivation does. The two must agree: a fold's bytes belong to
\ its own handle, not to whoever folded last.
\
\ Against the single shared accumulator this module used to have, they did not:
\ the overlapping run produced ONE key and handed the same wrong value back for
\ both folds. Restoring that behaviour - one slot, never released - turns these
\ equalities red and flips the "the two keys differ" guard to true, which is the
\ silent-mixing signature itself.
: CKT-FOLD-SEQUENTIAL ( -- )
   OPEN
   s" alpha-1" TEXT+
   s" alpha-2" TEXT+
   CKT-SEQ-A FINAL-HEX
   OPEN
   s" beta-1" TEXT+
   s" beta-2" TEXT+
   CKT-SEQ-B FINAL-HEX ;

: CKT-FOLD-OVERLAPPED ( -- )
   OPEN
   s" alpha-1" TEXT+
   OPEN
   s" beta-1" TEXT+
   swap
   s" alpha-2" TEXT+
   swap
   s" beta-2" TEXT+
   CKT-INT-B FINAL-HEX
   CKT-INT-A FINAL-HEX ;

: CKT-FOLD-OVERLAP-MATCHES ( -- )
   CKT-FOLD-SEQUENTIAL
   CKT-FOLD-OVERLAPPED
   CKT-SEQ-A CK-HEX-LEN CKT-INT-A CK-HEX-LEN STR= TTRUE
   CKT-SEQ-B CK-HEX-LEN CKT-INT-B CK-HEX-LEN STR= TTRUE
   \ and the two keys are genuinely different keys, so the equalities above are
   \ not both passing on one repeated value.
   CKT-SEQ-A CK-HEX-LEN CKT-SEQ-B CK-HEX-LEN STR= TFALSE
   CKT-INT-A CK-HEX-LEN CKT-INT-B CK-HEX-LEN STR= TFALSE ;

\ A handle is done when its key is taken: reusing it names a slot it no longer
\ owns, and that throws rather than folding into whatever holds the slot now.
\ The stale copy is parked in a typed cell because `catch` takes a ( -- )
\ quotation, so the retry cannot be handed its handle on the stack.
1 LAYOUT-BUFFER CKT-STALE-BUF fold

: CKT-STALE! ( fold -- )
   0 CKT-STALE-BUF ! ;

: CKT-STALE@ ( -- fold )
   0 CKT-STALE-BUF @ ;

: CKT-STALE-USE ( -- )
   CKT-STALE@ s" delta" TEXT+ drop ;

: CKT-FOLD-STALE-THROWS ( -- )
   OPEN dup CKT-STALE!
   s" gamma" TEXT+ CKT-SEQ-A FINAL-HEX
   [: CKT-STALE-USE ;] catch E-CK-STALE T= ;

\ Every fold slot is released by FINAL, so a run that opens and finishes folds
\ forever never exhausts the pool.
: CKT-FOLD-SLOTS-RECYCLE ( -- )
   0 begin dup FOLDS 2 * < while
      OPEN s" recycle" TEXT+ CKT-SEQ-A FINAL-HEX
      1+
   repeat drop
   0 FOLD-FILL 0 T= ;

: CKT-MAIN ( -- )
   T-RESET
   CKT-FOLD-OVERLAP-MATCHES
   CKT-FOLD-STALE-THROWS
   CKT-FOLD-SLOTS-RECYCLE
   CKT-SETUP
   CKT-CACHE-STABLE-HIT
   CKT-CACHE-INVALIDATES
   CKT-CACHE-BLOATED-COMPACTS
   CKT-CACHE-BACKCOMPAT
   CKT-CACHE-OVERCAP-REBUILDS
   CKT-CACHE-ATOMIC-POSTCOND
   CKT-PERSIST-TEMP-FAULT
   CKT-PERSIST-WRITE-FAULT
   CKT-PERSIST-RENAME-FAULT
   CKT-PERSIST-NOMASK
   CKT-PERSIST-RETRY
   CKT-PERSIST-REAL-MISSING-DIR
   CKT-CLEANUP
   T-REPORT ;

CKT-MAIN

;package
