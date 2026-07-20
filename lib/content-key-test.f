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
\ private capacity/flag/length cells (CK-CACHE-CAP, CK-CACHE-DISABLED, CK-HEX-LEN)
\ and call the public builders by their bare package-local names.
package CONTENT-KEY

$8000 constant CKT-READ-CAP
64 constant CKT-KEY-LEN
$8000 constant CKT-BUILD-CAP
CK-CACHE-CAP 4096 + constant CKT-BIG-CAP

variable CKT-ROOT-U
variable CKT-SRC-U
variable CKT-CACHE-U
variable CKT-BUILD-U
variable CKT-BIG-A
variable CKT-TMP-COUNT

create CKT-ROOT FS-PATH-CAP allot
create CKT-SRC FS-PATH-CAP allot
create CKT-CACHE FS-PATH-CAP allot
create CKT-KEY1 80 allot
create CKT-KEY2 80 allot
create CKT-READ CKT-READ-CAP allot
create CKT-DG 40 allot
create CKT-DGHEX 80 allot
create CKT-BUILD CKT-BUILD-CAP allot

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
   RESET
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
\ compacted rather than disabled. We assert the post-condition: still enabled,
\ file back under the cap, and lookups work.
: CKT-CACHE-OVERCAP-REBUILDS ( -- )
   CKT-SRC$ s" epsilon" WRITE-ALL
   CKT-CACHE$ CKT-BIG CKT-BIG-CAP WRITE-ALL
   CKT-REPOINT
   CKT-KEY1 CKT-KEY!
   CK-CACHE-DISABLED @ 0 T=
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

: CKT-CLEANUP ( -- )
   CACHE-CLEAR!
   CLEANUP-RUN
   CKT-ROOT$ EXISTS? TFALSE ;

: CKT-MAIN ( -- )
   T-RESET
   CKT-SETUP
   CKT-CACHE-STABLE-HIT
   CKT-CACHE-INVALIDATES
   CKT-CACHE-BLOATED-COMPACTS
   CKT-CACHE-BACKCOMPAT
   CKT-CACHE-OVERCAP-REBUILDS
   CKT-CACHE-ATOMIC-POSTCOND
   CKT-CLEANUP
   T-REPORT ;

CKT-MAIN

;package
