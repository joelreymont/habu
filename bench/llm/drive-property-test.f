\ drive-property-test.f - focused tests for native stdlib property driver.

: DPRT-CONFIG-COMMON ( -- )
   DTH-MODELS$ MR-REGISTRY!
   s" fixture" MR-REQUIRE
   s" -- bool" DS-SIG!
   s" property" DS-CATEGORY!
   s" empty -> -1" DS-TESTS!
   s" test-seed" DS-SEED!
   1 DS-TRIAL !
   10 DS-TASK-ORDER !
   2 DS-K !
   1 DS-MAX-REPAIRS ! ;

: DPRT-CONFIG-DEFAULTS ( -- )
   DPRT-CONFIG-COMMON
   112 DS-ID !
   s" PROP-DEFAULTS-OK?" DS-NAME!
   s" property defaults fixture" DS-SPEC! ;

: DPRT-CONFIG-RND ( -- )
   DPRT-CONFIG-COMMON
   113 DS-ID !
   s" PROP-RND-SEQ-OK?" DS-NAME!
   s" property random fixture" DS-SPEC! ;

: DPRT-CONFIG-GEN ( -- )
   DPRT-CONFIG-COMMON
   114 DS-ID !
   s" PROP-GEN-SCRIPT-OK?" DS-NAME!
   s" property generator fixture" DS-SPEC! ;

: DPRT-CONFIG-SHRINK ( -- )
   DPRT-CONFIG-COMMON
   115 DS-ID !
   s" PROP-SHRINK-OK?" DS-NAME!
   s" property shrink fixture" DS-SPEC! ;

: DPRT-CONFIG-BAD-SEED ( -- )
   DPRT-CONFIG-COMMON
   116 DS-ID !
   s" PROP-BAD-SEED" DS-NAME!
   s" -- error" DS-SIG!
   s" code E-PROP-SEED" DS-TESTS!
   s" property bad seed fixture" DS-SPEC! ;

T-RESET

DPRT-CONFIG-DEFAULTS
DTH-SRC-RESET
DTH-SRC-TASK-HEAD
s" PROP-DEFAULTS 250 = swap 1 = and " DTH-SRC+
DTH-SRC-END DPR-RUN-TEXT
LR-OUTCOME$ s" pass" T$=
LR-FIRST-CHECKER$ s" certified" T$=
LR-TESTS-PASSED @ -1 T=
s" arm" s" habu-stdlib-property" DTH-ROW-NEED-S
s" prompt_sha256" DTH-ROW-NEED-KEY
CLEANUP-RUN

DPRT-CONFIG-RND
DTH-SRC-RESET
DTH-SRC-TASK-HEAD
s" 1 5 PROP-RUN-RESET " DTH-SRC+
s" PROP-RND 1103527590 <> if STR-FALSE exit then " DTH-SRC+
s" PROP-SEED@ 1103527590 <> if STR-FALSE exit then " DTH-SRC+
s" 10 PROP-RND% 5 <> if STR-FALSE exit then " DTH-SRC+
s" PROP-COUNT@ 5 = " DTH-SRC+
DTH-SRC-END DPR-RUN-TEXT
LR-OUTCOME$ s" pass" T$=
LR-FIRST-CHECKER$ s" certified" T$=
LR-TESTS-PASSED @ -1 T=
s" arm" s" habu-stdlib-property" DTH-ROW-NEED-S
CLEANUP-RUN

DPRT-CONFIG-GEN
DTH-SRC-RESET
DTH-SRC-TASK-HEAD
s" 0 PROP-GEN-START " DTH-SRC+
s" 7 " DTH-SRC-S" s"  0 1 PROP-GEN-STEP " DTH-SRC+
s" drop " DTH-SRC-S" s"  1 -1 PROP-GEN-STEP " DTH-SRC+
s" PROP-GEN-DEPTH@ 0 <> if STR-FALSE exit then " DTH-SRC+
s" PROP-BUF$ " DTH-SRC+
s" 7 drop " DTH-SRC-S"
s"  STR= " DTH-SRC+
DTH-SRC-END DPR-RUN-TEXT
LR-OUTCOME$ s" pass" T$=
LR-FIRST-CHECKER$ s" certified" T$=
LR-TESTS-PASSED @ -1 T=
s" arm" s" habu-stdlib-property" DTH-ROW-NEED-S
CLEANUP-RUN

DPRT-CONFIG-SHRINK
DTH-SRC-RESET
s" : PROP-BENCH-KEEP? ( -- bool ) PROP-BUF$ nip 4 >= ; " DTH-SRC+
s" : PROP-SHRINK-OK? ( -- bool ) " DTH-SRC+
s" PROP-BUF-RESET " DTH-SRC+
s" dup drop 1+ " DTH-SRC-S" s"  PROP-BUF+ " DTH-SRC+
s" [: PROP-BENCH-KEEP? ;] PROP-SHRINK " DTH-SRC+
s" PROP-BUF$ " DTH-SRC+
s" dup " DTH-SRC-S"
s"  STR= " DTH-SRC+
DTH-SRC-END DPR-RUN-TEXT
LR-OUTCOME$ s" pass" T$=
LR-FIRST-CHECKER$ s" certified" T$=
LR-TESTS-PASSED @ -1 T=
s" arm" s" habu-stdlib-property" DTH-ROW-NEED-S
CLEANUP-RUN

DPRT-CONFIG-DEFAULTS
DTH-SRC-RESET
DTH-SRC-TASK-HEAD
s" -1 " DTH-SRC+
DTH-SRC-END DPR-RUN-TEXT
LR-OUTCOME$ s" reject" T$=
LR-FIRST-CHECKER$ s" rejected" T$=
LR-TESTS-PASSED @ 0 T=
s" required stdlib word missing" DTH-ROW-HAS
CLEANUP-RUN

DPRT-CONFIG-DEFAULTS
DTH-SRC-RESET
DTH-SRC-TASK-HEAD
s" 0 SCRIPT-ARGV$ drop drop -1 " DTH-SRC+
DTH-SRC-END DPR-RUN-TEXT
LR-OUTCOME$ s" reject" T$=
LR-FIRST-CHECKER$ s" rejected" T$=
LR-TESTS-PASSED @ 0 T=
s" forbidden property boundary" DTH-ROW-HAS
CLEANUP-RUN

DPRT-CONFIG-BAD-SEED
DTH-SRC-RESET
s" : PROP-BAD-SEED ( -- ) -1 1 PROP-RUN-RESET ;" DTH-SRC+
DTH-SRC-END DPR-RUN-TEXT
LR-OUTCOME$ s" reject" T$=
LR-FIRST-CHECKER$ s" rejected" T$=
LR-TESTS-PASSED @ 0 T=
s" code E-PROP-SEED" DTH-ROW-HAS
CLEANUP-RUN

T-REPORT
s" drive-property-test: ok" type cr
