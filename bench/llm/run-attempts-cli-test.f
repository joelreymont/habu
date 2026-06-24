\ run-attempts-cli-test.f - end-to-end checked attempt CLI fixture.
\
\ Load after bench/llm/run-attempts.f.

120000 constant RACT-TIMEOUT-MS
$20000 constant RACT-CAP
10 constant RACT-LF

create RACT-ROOT FS-PATH-CAP allot
create RACT-CAND FS-PATH-CAP allot
create RACT-OUT-PATH FS-PATH-CAP allot
create RACT-TASKS FS-PATH-CAP allot
create RACT-SOLUTIONS FS-PATH-CAP allot
create RACT-TESTS FS-PATH-CAP allot
create RACT-SRC FS-PATH-CAP allot
create RACT-OUT RACT-CAP allot
create RACT-ERR RACT-CAP allot
create RACT-JSONL RACT-CAP allot

variable RACT-ROOT-U
variable RACT-CAND-U
variable RACT-OUT-PATH-U
variable RACT-TASKS-U
variable RACT-SOLUTIONS-U
variable RACT-TESTS-U
variable RACT-SRC-U
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

: RACT-TASKS$ ( -- ptr u8 n )
   RACT-TASKS RACT-TASKS-U @ ;

: RACT-SOLUTIONS$ ( -- ptr u8 n )
   RACT-SOLUTIONS RACT-SOLUTIONS-U @ ;

: RACT-TESTS$ ( -- ptr u8 n )
   RACT-TESTS RACT-TESTS-U @ ;

: RACT-SRC$ ( -- ptr u8 n )
   RACT-SRC RACT-SRC-U @ ;

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
   s" tasks.tsv" RACT-TASKS RACT-TASKS-U RACT-ROOT-JOIN!
   s" solutions.f" RACT-SOLUTIONS RACT-SOLUTIONS-U RACT-ROOT-JOIN!
   s" tests.f" RACT-TESTS RACT-TESTS-U RACT-ROOT-JOIN!
   RACT-CAND$ MAKE-DIRS ;

: RACT-TASKS-TEXT$ ( -- ptr u8 n )
   s" id	name	signature	category	tests	harness	conv	spec	vectors	tags	js_signature	rust_signature
9	SWAP2	(a b -- b a)	polymorphic	1 2 -> 2 1	forth	stack	Define SWAP2 with the checked Forth stack effect.	-	polymorphic,forth	-	-
" ;

: RACT-SOLUTIONS-TEXT$ ( -- ptr u8 n )
   s" : SWAP2 ( a b -- b a ) swap ;
" ;

: RACT-TESTS-TEXT$ ( -- ptr u8 n )
   s" : ASSERT= ( n n -- ) <> IF 1 die THEN ;
1 2 SWAP2 1 ASSERT= 2 ASSERT=
111 emit 107 emit 10 emit
" ;

: RACT-WRITE-FIXTURE-FILES ( -- )
   RACT-TASKS$ RACT-TASKS-TEXT$ WRITE-ALL
   RACT-SOLUTIONS$ RACT-SOLUTIONS-TEXT$ WRITE-ALL
   RACT-TESTS$ RACT-TESTS-TEXT$ WRITE-ALL ;

: RACT-CAND-SRC! ( ptr u8 n -- ) {: id:ptr idu :}
   RACT-CAND$ id idu RACT-SRC RACT-SRC-U RA-DIR-ID-FILE! ;

: RACT-WEAK9$ ( -- ptr u8 n )
   s" : SWAP2 ( n n -- n n ) swap ;" ;

: RACT-WRITE-WEAK9 ( -- )
   s" 9" RACT-CAND-SRC!
   RACT-SRC$ RACT-WEAK9$ WRITE-ALL ;

: RACT-MATERIALIZE-CANDIDATES ( -- )
   RACT-WRITE-FIXTURE-FILES
   RACT-TASKS$ RACT-SOLUTIONS$ RACT-CAND$ AS-EXTRACT-FILES
   RACT-WRITE-WEAK9 ;

: RACT-CLI-ARGS ( -- )
   PROC-ARGV-ENV-RESET
   PROC-ENV-INHERIT-MISSING
   s" --load"  >LEN PROC-ARGV+
   s" lib/errors.f"  >LEN PROC-ARGV+
   s" lib/string.f"  >LEN PROC-ARGV+
   s" lib/memory.f"  >LEN PROC-ARGV+
   s" lib/json-write.f"  >LEN PROC-ARGV+
   s" tools/json.f"  >LEN PROC-ARGV+
   s" lib/fs.f"  >LEN PROC-ARGV+
   s" lib/fs-mutate.f"  >LEN PROC-ARGV+
   s" lib/process.f"  >LEN PROC-ARGV+
   s" lib/process-argv.f"  >LEN PROC-ARGV+
   s" lib/process-env.f"  >LEN PROC-ARGV+
   s" lib/time.f"  >LEN PROC-ARGV+
   s" lib/date.f"  >LEN PROC-ARGV+
   s" lib/vector.f"  >LEN PROC-ARGV+
   s" bench/llm/manifest.f"  >LEN PROC-ARGV+
   s" tools/lint/text.f"  >LEN PROC-ARGV+ s" tools/lint/token.f" >LEN PROC-ARGV+ s" tools/lint/lib.f" >LEN PROC-ARGV+
   s" tools/lint/source-lex.f"  >LEN PROC-ARGV+
   s" tools/argv.f"  >LEN PROC-ARGV+
   s" bench/llm/forth-task-lines-lib.f"  >LEN PROC-ARGV+
   s" bench/llm/attempt-solutions-lib.f"  >LEN PROC-ARGV+
   s" bench/llm/diagnostic-stats.f"  >LEN PROC-ARGV+
   s" bench/llm/run-attempts-lib.f"  >LEN PROC-ARGV+
   s" bench/llm/run-attempts.f"  >LEN PROC-ARGV+
   s" --"  >LEN PROC-ARGV+
   RACT-CAND$  >LEN PROC-ARGV+
   RACT-OUT-PATH$  >LEN PROC-ARGV+
   s" attempt-fixture-2026-06-16"  >LEN PROC-ARGV+
   s" fixture-model"  >LEN PROC-ARGV+
   RACT-TASKS$  >LEN PROC-ARGV+
   RACT-SOLUTIONS$  >LEN PROC-ARGV+
   RACT-TESTS$  >LEN PROC-ARGV+ ;

: RACT-RUN-CLI ( -- )
   RACT-CLI-ARGS
   s" bin/hb" >LEN RACT-OUT RACT-CAP >LEN
   RACT-ERR RACT-CAP >LEN RACT-TIMEOUT-MS >MS
   RUN-ARGV-ENV-CAPTURE {: outu erru rc :}
   rc RC>N RACT-RC !
   erru LEN>N RACT-ERR-U !
   outu LEN>N RACT-OUT-U ! ;

: RACT-READ-JSONL ( -- )
   RACT-OUT-PATH$ RACT-JSONL RACT-CAP READ-ALL RACT-JSONL-U ! ;

: RACT-FORTH-TASK-COUNT ( -- n )
   RACT-TASKS$ FTL-FILE$ RACT-LF COUNT-CHAR ;

: RACT-U+ ( n -- ) {: n :}
   n 0 < if E-STR-BOUNDS throw then
   n 10 >= if n 10 / RECURSE then
   n 10 mod STR-ZERO + SB-APPEND-C ;

: RACT-SUMMARY$ ( n -- ptr u8 n ) {: rows :}
   SB-RESET
   s" run=attempt-fixture-2026-06-16 model=fixture-model rows=" SB-APPEND
   rows RACT-U+
   s"  certified=" SB-APPEND
   1 RACT-U+
   s"  first_tests=" SB-APPEND
   1 RACT-U+
   s"  tests=" SB-APPEND
   1 RACT-U+
   s"  repairs=0 checker_iterations=1 diagnostics=0 tokens=0" SB-APPEND
   SB$ ;

: RACT-EXPECT-SUMMARY ( n -- ) {: rows :}
   RACT-ERR$ rows RACT-SUMMARY$ CONTAINS? TTRUE
   RACT-ERR$ s" buckets checker_rejected=0 checker_false_rejects=0 checker_model_rejected=0 first_tests_failed=0 tests_failed=0 trust_used=0 signature_weakened=1" CONTAINS? TTRUE
   RACT-ERR$ s" category polymorphic rows=1 certified=1 tests=1" CONTAINS? TTRUE
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
   RACT-EXPECT-TASK9
   CLEANUP-RUN
   RACT-ROOT$ EXISTS? TFALSE
   T-REPORT
   s" run-attempts-cli-test: ok" type cr ;

RACT-MAIN
