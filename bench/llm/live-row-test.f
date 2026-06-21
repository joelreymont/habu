\ live-row-test.f - focused tests for bench/llm/live-row.f.

create LRT-ROOT FS-PATH-CAP allot
create LRT-PROMPT FS-PATH-CAP allot
create LRT-RAW FS-PATH-CAP allot
create LRT-CAND FS-PATH-CAP allot
create LRT-DIAG FS-PATH-CAP allot
create LRT-REPAIR FS-PATH-CAP allot
create LRT-TEST FS-PATH-CAP allot
create LRT-BUNDLE FS-PATH-CAP allot

variable LRT-ROOT-U
variable LRT-PROMPT-U
variable LRT-RAW-U
variable LRT-CAND-U
variable LRT-DIAG-U
variable LRT-REPAIR-U
variable LRT-TEST-U
variable LRT-BUNDLE-U

: LRT-COPY! ( ptr u8 n ptr u8 ptr n -- ) {: a:ptr u dst:ptr up:ptr :}
   u FS-PATH-CAP > if E-FS-CAPACITY throw then
   a dst u BYTE-COPY
   u up ! ;

: LRT-ROOT$ ( -- ptr u8 n )
   LRT-ROOT LRT-ROOT-U @ ;

: LRT-TEMP ( -- )
   s" habu-live-row-test" TMPDIR-MKDIR LRT-ROOT LRT-ROOT-U LRT-COPY!
   LRT-ROOT$ CLEANUP-TREE+ ;

: LRT-WRITE ( ptr u8 n ptr u8 n ptr u8 ptr n -- )
   {: name:ptr nameu body:ptr bodyu dst:ptr up:ptr :}
   LRT-ROOT$ name nameu dst JOIN-PATH {: pathu :}
   pathu up !
   dst pathu body bodyu WRITE-ALL ;

: LRT-FILES ( -- )
   s" prompt.txt" s" Prompt" LRT-PROMPT LRT-PROMPT-U LRT-WRITE
   s" raw.txt" s" Raw" LRT-RAW LRT-RAW-U LRT-WRITE
   s" cand.f" s" : DATE-PARSE-OK? ( -- bool ) -1 ;" LRT-CAND LRT-CAND-U LRT-WRITE
   s" diag.txt" s" " LRT-DIAG LRT-DIAG-U LRT-WRITE
   s" repair.json" s" " LRT-REPAIR LRT-REPAIR-U LRT-WRITE
   s" test.txt" s" ok" LRT-TEST LRT-TEST-U LRT-WRITE
   s" bundle.f" s" : DATE-PARSE-OK? ( -- bool ) -1 ; DATE-PARSE-OK? . cr" LRT-BUNDLE LRT-BUNDLE-U LRT-WRITE ;

: LRT-CONTAINS ( ptr u8 n ptr u8 n -- )
   CONTAINS? TTRUE ;

: LRT-CONFIG ( -- )
   LR-RESET
   s" run" LR-RUN-ID!
   62 LR-TASK-ID !
   s" DATE-PARSE-OK?" LR-NAME!
   s" fixture" LR-MODEL-ID!
   s" Fixture" LR-MODEL!
   s" habu-stdlib" LR-ARM!
   1 LR-TRIAL !
   7 LR-TASK-ORDER !
   2 LR-K !
   s" seed" LR-SEED!
   s" pass" LR-OUTCOME!
   1 LR-ROUNDS !
   -1 LR-FIRST-PASS !
   12 LR-TOKENS !
   34 LR-WALL-MS !
   35 LR-SOURCE-CHARS !
   s" date" LR-FAMILY!
   s" test-version" LR-MODEL-VERSION!
   s" 2026-06-20" LR-MODEL-DATE!
   s" certified" LR-FIRST-CHECKER!
   -1 LR-FIRST-TESTS !
   -1 LR-TESTS-PASSED !
   1 LR-CHECKER-ITERATIONS !
   LRT-PROMPT LRT-PROMPT-U @ LR-PROMPT!
   LRT-RAW LRT-RAW-U @ LR-RAW!
   LRT-CAND LRT-CAND-U @ LR-CANDIDATE!
   LRT-DIAG LRT-DIAG-U @ LR-DIAGNOSTICS!
   LRT-REPAIR LRT-REPAIR-U @ LR-REPAIR!
   LRT-TEST LRT-TEST-U @ LR-TEST!
   LRT-BUNDLE LRT-BUNDLE-U @ LR-BUNDLE! ;

: LRT-ASSERT-ROW ( ptr u8 n -- )
   2dup s" schema_version" LRT-CONTAINS
   2dup s" seed:fixture:habu-stdlib:62:1" LRT-CONTAINS
   2dup s" task_family" LRT-CONTAINS
   2dup s" repair_class_stats" LRT-CONTAINS
   2dup s" []" LRT-CONTAINS
   2dup s" Prompt" LRT-CONTAINS
   2dup s" prompt_sha256" LRT-CONTAINS
   2dup s" final_bundle" LRT-CONTAINS
   s" tests_passed" LRT-CONTAINS ;

: LRT-CONFIG-DIAG ( -- )
   LRT-CONFIG
   s" reject" LR-OUTCOME!
   s" rejected" LR-FIRST-CHECKER!
   0 LR-FIRST-PASS !
   0 LR-FIRST-TESTS !
   0 LR-TESTS-PASSED !
   1 LR-DIAG-COUNT !
   s" fixture_diag" LR-REPAIR-CLASS! ;

: LRT-ASSERT-DIAG-ROW ( ptr u8 n -- )
   2dup s" fixture_diag" LRT-CONTAINS
   2dup s" diagnostic_count" LRT-CONTAINS
   2dup s" repair_success" LRT-CONTAINS
   s" token_delta" LRT-CONTAINS ;

: LRT-MAIN ( -- )
   T-RESET
   CLEANUP-RESET
   LRT-TEMP
   LRT-FILES
   LRT-CONFIG
   LR-ROW$ LRT-ASSERT-ROW
   LRT-CONFIG-DIAG
   LR-ROW$ LRT-ASSERT-DIAG-ROW
   CLEANUP-RUN
   T-REPORT
   s" live-row-test: ok" type cr ;

LRT-MAIN
