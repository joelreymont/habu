\ drive-file-test.f - focused tests for native stdlib file driver.

: DFT-CONFIG-COMMON ( -- )
   DTH-MODELS$ MR-REGISTRY!
   s" fixture" MR-REQUIRE
   s" -- bool" DS-SIG!
   s" files" DS-CATEGORY!
   s" temp path -> -1" DS-TESTS!
   s" test-seed" DS-SEED!
   1 DS-TRIAL !
   8 DS-TASK-ORDER !
   2 DS-K !
   1 DS-MAX-REPAIRS ! ;

: DFT-CONFIG-READ ( -- )
   DFT-CONFIG-COMMON
   103 DS-ID !
   s" FS-READ-ALL-OK?" DS-NAME!
   s" Using READ-ALL, read a temp fixture file into a bounded byte buffer and compare its contents." DS-SPEC! ;

: DFT-CONFIG-WRITE ( -- )
   DFT-CONFIG-COMMON
   104 DS-ID !
   s" FS-WRITE-ALL-OK?" DS-NAME!
   s" Using WRITE-ALL and READ-ALL, write a temp file and read it back from a bounded byte buffer." DS-SPEC! ;

: DFT-CONFIG-APPEND ( -- )
   DFT-CONFIG-COMMON
   105 DS-ID !
   s" FS-APPEND-OK?" DS-NAME!
   s" Using APPEND-FILE, append to a temp file and verify the resulting contents." DS-SPEC! ;

: DFT-CONFIG-CAPACITY ( -- )
   DFT-CONFIG-COMMON
   106 DS-ID !
   s" FS-READ-CAPACITY" DS-NAME!
   s" -- error" DS-SIG!
   s" code E-FS-CAPACITY" DS-TESTS!
   s" Reject READ-ALL into a byte buffer smaller than the file instead of truncating silently." DS-SPEC! ;

: DFT-SRC-READ-WANT ( -- )
   s" FS-FIX-BUF swap FS-FIX-READ-WANT$ STR= " DTH-SRC+ ;

: DFT-SRC-WRITE-WANT ( -- )
   s" FS-FIX-BUF swap FS-FIX-WRITE-DATA$ STR= " DTH-SRC+ ;

: DFT-SRC-APPEND-WANT ( -- )
   s" FS-FIX-BUF swap FS-FIX-APPEND-WANT$ STR= " DTH-SRC+ ;

: DFT-GOOD-READ$ ( -- ptr u8 n )
   DTH-SRC-RESET
   DTH-SRC-TASK-HEAD
   s" FS-FIX-READ-PATH$ FS-FIX-BUF FS-FIX-CAP READ-ALL " DTH-SRC+
   DFT-SRC-READ-WANT
   DTH-SRC-END ;

: DFT-GOOD-WRITE$ ( -- ptr u8 n )
   DTH-SRC-RESET
   DTH-SRC-TASK-HEAD
   s" FS-FIX-WRITE-PATH$ FS-FIX-WRITE-DATA$ WRITE-ALL " DTH-SRC+
   s" FS-FIX-WRITE-PATH$ FS-FIX-BUF FS-FIX-CAP READ-ALL " DTH-SRC+
   DFT-SRC-WRITE-WANT
   DTH-SRC-END ;

: DFT-GOOD-APPEND$ ( -- ptr u8 n )
   DTH-SRC-RESET
   DTH-SRC-TASK-HEAD
   s" FS-FIX-APPEND-PATH$ FS-FIX-APPEND-DATA$ APPEND-FILE " DTH-SRC+
   s" FS-FIX-APPEND-PATH$ FS-FIX-BUF FS-FIX-CAP READ-ALL " DTH-SRC+
   DFT-SRC-APPEND-WANT
   DTH-SRC-END ;

: DFT-GOOD-CAPACITY$ ( -- ptr u8 n )
   DTH-SRC-RESET
   \ Normal success effect; FS-FIX-SMALL-CAP makes READ-ALL throw first.
   s" : FS-READ-CAPACITY ( -- n ) " DTH-SRC+
   s" FS-FIX-BIG-PATH$ FS-FIX-BUF FS-FIX-SMALL-CAP READ-ALL " DTH-SRC+
   DTH-SRC-END ;

: DFT-SCRIPT-ARGV$ ( -- ptr u8 n )
   DTH-SRC-RESET
   DTH-SRC-TASK-HEAD
   s" 0 SCRIPT-ARGV$ FS-FIX-BUF FS-FIX-CAP READ-ALL " DTH-SRC+
   DFT-SRC-READ-WANT
   DTH-SRC-END ;

: DFT-CONSTANT$ ( -- ptr u8 n )
   DTH-SRC-RESET
   DTH-SRC-TASK-HEAD
   s" -1 " DTH-SRC+
   DTH-SRC-END ;

: DFT-ASSERT-PASS ( -- )
   LR-OUTCOME$ s" pass" T$=
   LR-FIRST-CHECKER$ s" certified" T$=
   LR-TESTS-PASSED @ -1 T=
   s" arm" s" habu-stdlib-file" DTH-ROW-NEED-S
   s" prompt_sha256" DTH-ROW-NEED-KEY
   s" final_bundle_sha256" DTH-ROW-NEED-KEY ;

: DFT-ASSERT-REJECT ( -- )
   LR-OUTCOME$ s" reject" T$=
   LR-FIRST-CHECKER$ s" rejected" T$=
   LR-TESTS-PASSED @ 0 T=
   s" required stdlib word missing" DTH-ROW-HAS ;

: DFT-ASSERT-FORBIDDEN ( -- )
   LR-OUTCOME$ s" reject" T$=
   LR-FIRST-CHECKER$ s" rejected" T$=
   LR-TESTS-PASSED @ 0 T=
   s" forbidden fixture boundary" DTH-ROW-HAS ;

T-RESET

DFT-CONFIG-READ
DFT-GOOD-READ$ DF-RUN-TEXT
DFT-ASSERT-PASS
CLEANUP-RUN

DFT-CONFIG-WRITE
DFT-GOOD-WRITE$ DF-RUN-TEXT
DFT-ASSERT-PASS
CLEANUP-RUN

DFT-CONFIG-APPEND
DFT-GOOD-APPEND$ DF-RUN-TEXT
DFT-ASSERT-PASS
CLEANUP-RUN

DFT-CONFIG-READ
DFT-CONSTANT$ DF-RUN-TEXT
DFT-ASSERT-REJECT
CLEANUP-RUN

DFT-CONFIG-READ
DFT-SCRIPT-ARGV$ DF-RUN-TEXT
DFT-ASSERT-FORBIDDEN
CLEANUP-RUN

DFT-CONFIG-CAPACITY
DFT-GOOD-CAPACITY$ DF-RUN-TEXT
LR-OUTCOME$ s" reject" T$=
LR-FIRST-CHECKER$ s" rejected" T$=
LR-TESTS-PASSED @ 0 T=
s" code E-FS-CAPACITY" DTH-ROW-HAS
CLEANUP-RUN

T-REPORT
s" drive-file-test: ok" type cr
