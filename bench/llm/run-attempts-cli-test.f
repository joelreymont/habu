\ run-attempts-cli-test.f - end-to-end checked attempt CLI fixture.
\
\ Load after bench/llm/run-attempts.f.

120000 constant RACT-TIMEOUT-MS
$20000 constant RACT-CAP
10 constant RACT-LF

create RACT-ROOT FS-PATH-CAP allot
create RACT-CAND FS-PATH-CAP allot
create RACT-OUT-PATH FS-PATH-CAP allot
create RACT-DIR FS-PATH-CAP allot
create RACT-SRC FS-PATH-CAP allot
create RACT-DST FS-PATH-CAP allot
create RACT-OUT RACT-CAP allot
create RACT-ERR RACT-CAP allot
create RACT-JSONL RACT-CAP allot

variable RACT-ROOT-U
variable RACT-CAND-U
variable RACT-OUT-PATH-U
variable RACT-DIR-U
variable RACT-SRC-U
variable RACT-DST-U
variable RACT-OUT-U
variable RACT-ERR-U
variable RACT-JSONL-U
variable RACT-RC
variable RACT-NEXT

: RACT-COPY! ( ptr u8 n ptr u8 ptr n -- ) {: a:ptr u dst:ptr up:ptr :}
   u FS-PATH-CAP > if E-FS-CAPACITY throw then
   a dst u BYTE-COPY
   u up ! ;

: RACT-ROOT$ ( -- ptr u8 n )
   RACT-ROOT RACT-ROOT-U @ ;

: RACT-CAND$ ( -- ptr u8 n )
   RACT-CAND RACT-CAND-U @ ;

: RACT-OUT-PATH$ ( -- ptr u8 n )
   RACT-OUT-PATH RACT-OUT-PATH-U @ ;

: RACT-DIR$ ( -- ptr u8 n )
   RACT-DIR RACT-DIR-U @ ;

: RACT-SRC$ ( -- ptr u8 n )
   RACT-SRC RACT-SRC-U @ ;

: RACT-DST$ ( -- ptr u8 n )
   RACT-DST RACT-DST-U @ ;

: RACT-ERR$ ( -- ptr u8 n )
   RACT-ERR RACT-ERR-U @ ;

: RACT-JSONL$ ( -- ptr u8 n )
   RACT-JSONL RACT-JSONL-U @ ;

: RACT-ROOT-JOIN! ( ptr u8 n ptr u8 ptr n -- ) {: name:ptr nameu dst:ptr up:ptr :}
   RACT-ROOT$ name nameu dst JOIN-PATH up ! ;

: RACT-PREPARE ( -- )
   CLEANUP-RESET
   s" habu-run-attempts-cli" TMPDIR-MKDIR RACT-ROOT RACT-ROOT-U RACT-COPY!
   RACT-ROOT$ CLEANUP-TREE+
   s" candidates" RACT-CAND RACT-CAND-U RACT-ROOT-JOIN!
   s" attempt.jsonl" RACT-OUT-PATH RACT-OUT-PATH-U RACT-ROOT-JOIN!
   RACT-CAND$ MAKE-DIRS ;

: RACT-CAND-SRC! ( ptr u8 n -- ) {: id:ptr idu :}
   RACT-CAND$ id idu RACT-SRC RACT-SRC-U RA-DIR-ID-FILE! ;

: RACT-CAND-DIR! ( ptr u8 n -- ) {: id:ptr idu :}
   RACT-CAND$ id idu RACT-DIR JOIN-PATH RACT-DIR-U ! ;

: RACT-ROUND-DST! ( ptr u8 n -- ) {: name:ptr nameu :}
   RACT-DIR$ name nameu RACT-DST JOIN-PATH RACT-DST-U ! ;

: RACT-WRITE-ROUND ( ptr u8 n ptr u8 n -- ) {: name:ptr nameu src:ptr srcu :}
   name nameu RACT-ROUND-DST!
   RACT-DST$ src srcu WRITE-ALL ;

: RACT-COPY-ROUND ( ptr u8 n -- ) {: name:ptr nameu :}
   name nameu RACT-ROUND-DST!
   RACT-SRC$ RACT-DST$ COPY-FILE-STREAM ;

: RACT-REPAIR ( ptr u8 n ptr u8 n -- ) {: id:ptr idu bad:ptr badu :}
   id idu RACT-CAND-SRC!
   id idu RACT-CAND-DIR!
   RACT-DIR$ MAKE-DIR
   s" 1.f" bad badu RACT-WRITE-ROUND
   s" 2.f" RACT-COPY-ROUND
   RACT-SRC$ REMOVE-FILE ;

: RACT-BAD1$ ( -- ptr u8 n )
   s" : SQUARE ( i64 -- i64 ) dup ;" ;

: RACT-BAD2$ ( -- ptr u8 n )
   s" : CUBE ( i64 -- i64 ) drop ;" ;

: RACT-BAD3$ ( -- ptr u8 n )
   s" : ABSV ( i64 -- i64 ) 0= ;" ;

: RACT-BAD4$ ( -- ptr u8 n )
   s" : NEG? ( i64 -- ) >r ;" ;

: RACT-BAD5$ ( -- ptr u8 n )
   s" : CLAMP0 ( i64 -- i64 ) evaluate ;" ;

: RACT-BAD6$ ( -- ptr u8 n )
   s" : SUM3 ( i64 i64 i64 ) + + ;" ;

: RACT-BAD7$ ( -- ptr u8 n )
   s" : AVG2 ( i64 i64 -- i64 ) leave ;" ;

: RACT-BAD8$ ( -- ptr u8 n )
   s" : MAX2 ( i64 -- i64 ) drop ;
: EXTRA ( i64 -- i64 ) dup ;" ;

: RACT-WEAK9$ ( -- ptr u8 n )
   s" : SWAP2 ( n n -- n n ) swap ;" ;

: RACT-WRITE-WEAK9 ( -- )
   s" 9" RACT-CAND-SRC!
   RACT-SRC$ RACT-WEAK9$ WRITE-ALL ;

: RACT-MATERIALIZE-CANDIDATES ( -- )
   s" bench/llm/tasks.tsv" s" bench/llm/solutions.f" RACT-CAND$ AS-EXTRACT-FILES
   s" 1" RACT-BAD1$ RACT-REPAIR
   s" 2" RACT-BAD2$ RACT-REPAIR
   s" 3" RACT-BAD3$ RACT-REPAIR
   s" 4" RACT-BAD4$ RACT-REPAIR
   s" 5" RACT-BAD5$ RACT-REPAIR
   s" 6" RACT-BAD6$ RACT-REPAIR
   s" 7" RACT-BAD7$ RACT-REPAIR
   s" 8" RACT-BAD8$ RACT-REPAIR
   RACT-WRITE-WEAK9 ;

: RACT-CLI-ARGS ( -- )
   PROC-ARGV-ENV-RESET
   PROC-ENV-INHERIT-MISSING
   s" --load" PROC-ARGV+
   s" lib/errors.f" PROC-ARGV+
   s" lib/string.f" PROC-ARGV+
   s" lib/json-write.f" PROC-ARGV+
   s" tools/json.f" PROC-ARGV+
   s" lib/fs.f" PROC-ARGV+
   s" lib/fs-mutate.f" PROC-ARGV+
   s" lib/process.f" PROC-ARGV+
   s" lib/process-argv.f" PROC-ARGV+
   s" lib/process-env.f" PROC-ARGV+
   s" lib/time.f" PROC-ARGV+
   s" lib/date.f" PROC-ARGV+
   s" bench/llm/manifest.f" PROC-ARGV+
   s" tools/lint/lib.f" PROC-ARGV+
   s" tools/lint/source-lex.f" PROC-ARGV+
   s" tools/argv.f" PROC-ARGV+
   s" bench/llm/forth-task-lines-lib.f" PROC-ARGV+
   s" bench/llm/attempt-solutions-lib.f" PROC-ARGV+
   s" bench/llm/diagnostic-stats.f" PROC-ARGV+
   s" bench/llm/run-attempts-lib.f" PROC-ARGV+
   s" bench/llm/run-attempts.f" PROC-ARGV+
   s" --" PROC-ARGV+
   RACT-CAND$ PROC-ARGV+
   RACT-OUT-PATH$ PROC-ARGV+
   s" attempt-fixture-2026-06-16" PROC-ARGV+
   s" fixture-model" PROC-ARGV+ ;

: RACT-RUN-CLI ( -- )
   RACT-CLI-ARGS
   s" bin/hb" RACT-OUT RACT-CAP RACT-ERR RACT-CAP RACT-TIMEOUT-MS RUN-ARGV-ENV-CAPTURE
   RACT-RC !
   RACT-ERR-U !
   RACT-OUT-U ! ;

: RACT-READ-JSONL ( -- )
   RACT-OUT-PATH$ RACT-JSONL RACT-CAP READ-ALL RACT-JSONL-U ! ;

: RACT-FORTH-TASK-COUNT ( -- n )
   s" bench/llm/tasks.tsv" FTL-FILE$ RACT-LF COUNT-CHAR ;

: RACT-U+ ( n -- ) {: n :}
   n 0 < if E-STR-BOUNDS throw then
   n 10 >= if n 10 / RECURSE then
   n 10 mod STR-ZERO + SB-APPEND-C ;

: RACT-SUMMARY$ ( n -- ptr u8 n ) {: rows :}
   SB-RESET
   s" run=attempt-fixture-2026-06-16 model=fixture-model rows=" SB-APPEND
   rows RACT-U+
   s"  certified=" SB-APPEND
   rows 8 - RACT-U+
   s"  first_tests=" SB-APPEND
   rows 8 - RACT-U+
   s"  tests=" SB-APPEND
   rows RACT-U+
   s"  repairs=8 checker_iterations=" SB-APPEND
   rows 8 + RACT-U+
   s"  diagnostics=9 tokens=0" SB-APPEND
   SB$ ;

: RACT-EXPECT-SUMMARY ( n -- ) {: rows :}
   RACT-ERR$ rows RACT-SUMMARY$ CONTAINS? TTRUE
   RACT-ERR$ s" buckets checker_rejected=8 checker_false_rejects=0 checker_model_rejected=8 first_tests_failed=8 tests_failed=0 trust_used=0 signature_weakened=1" CONTAINS? TTRUE
   RACT-ERR$ s" repair_class remove_producer rows=2 repair_success=2 repair_iterations=2 diagnostics=2 token_delta=0" CONTAINS? TTRUE
   RACT-ERR$ s" repair_class add_producer rows=2 repair_success=2 repair_iterations=2 diagnostics=2 token_delta=0" CONTAINS? TTRUE
   RACT-ERR$ s" run-attempts: wrote " CONTAINS? TTRUE ;

: RACT-JGET ( n ptr u8 n -- n ) {: root key:ptr keyu :}
   root key keyu JSON-GET dup -1 = if E-RA-MISSING throw then ;

: RACT-U-FIELD ( n ptr u8 n -- n ) {: root key:ptr keyu :}
   root key keyu RACT-JGET JSON-NUMBER$ STR>NUMBER? 0= if E-RA-MISSING throw then ;

: RACT-U-FIELD= ( n ptr u8 n n -- ) {: root key:ptr keyu want :}
   root key keyu RACT-U-FIELD want T= ;

: RACT-S-FIELD= ( n ptr u8 n ptr u8 n -- ) {: root key:ptr keyu want:ptr wantu :}
   root key keyu RACT-JGET JSON-STRING$ want wantu T$= ;

: RACT-BOOL-FIELD= ( n ptr u8 n bool -- ) {: root key:ptr keyu want :}
   root key keyu RACT-JGET JSON-BOOL@ if
      want TTRUE
   else
      want TFALSE
   then ;

: RACT-TASK-ID@ ( n -- n )
   s" task_id" RACT-U-FIELD ;

: RACT-FIND-TASK ( n -- n ) {: want :}
   0 RACT-NEXT !
   begin RACT-JSONL RACT-JSONL-U @ RACT-NEXT @ BM-LINE-NEXT while
      RACT-NEXT !
      JSON-PARSE dup RACT-TASK-ID@ want = if exit then drop
   repeat drop 2drop
   E-RA-MISSING throw ;

: RACT-ARR-FIELD ( n ptr u8 n -- n )
   RACT-JGET dup JSON-KIND J-ARR T= ;

: RACT-EXPECT-TASK1 ( -- )
   1 RACT-FIND-TASK {: root :}
   root s" first_pass_checker" s" rejected" RACT-S-FIELD=
   root s" tests_passed" STR-TRUE RACT-BOOL-FIELD=
   root s" repair_iterations" 1 RACT-U-FIELD=
   root s" diagnostic_count" 1 RACT-U-FIELD= ;

: RACT-EXPECT-TASK8 ( -- )
   8 RACT-FIND-TASK {: root :}
   root s" diagnostic_count" 2 RACT-U-FIELD=
   root s" repair_class_stats" RACT-ARR-FIELD {: stats :}
   stats JSON-COUNT 2 T=
   stats 0 JSON-ARR@ s" repair_class" s" remove_producer" RACT-S-FIELD=
   stats 1 JSON-ARR@ s" repair_class" s" add_producer" RACT-S-FIELD= ;

: RACT-EXPECT-TASK9 ( -- )
   9 RACT-FIND-TASK {: root :}
   root s" first_pass_checker" s" certified" RACT-S-FIELD=
   root s" signature_weakened" STR-TRUE RACT-BOOL-FIELD= ;

: RACT-MAIN ( -- )
   T-RESET
   RACT-PREPARE
   RACT-MATERIALIZE-CANDIDATES
   RACT-RUN-CLI
   RACT-RC @ 0 T=
   RACT-OUT-U @ 0 T=
   RACT-READ-JSONL
   RACT-FORTH-TASK-COUNT {: rows :}
   RACT-JSONL$ RACT-LF COUNT-CHAR rows T=
   rows RACT-EXPECT-SUMMARY
   RACT-EXPECT-TASK1
   RACT-EXPECT-TASK8
   RACT-EXPECT-TASK9
   CLEANUP-RUN
   RACT-ROOT$ EXISTS? TFALSE
   T-REPORT
   s" run-attempts-cli-test: ok" type cr ;

RACT-MAIN
