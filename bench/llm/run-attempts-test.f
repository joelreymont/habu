\ run-attempts-test.f - focused tests for attempt runner helpers.

create RATT-ROOT FS-PATH-CAP allot
create RATT-PATH FS-PATH-CAP allot
create RATT-DIR FS-PATH-CAP allot
create RATT-REF FS-PATH-CAP allot
create RATT-CAND FS-PATH-CAP allot
create RATT-TESTS FS-PATH-CAP allot
create RATT-EXP RA-SRC-CAP allot

variable RATT-ROOT-U
variable RATT-PATH-U
variable RATT-DIR-U
variable RATT-REF-U
variable RATT-CAND-U
variable RATT-TESTS-U
variable RATT-EXP-U

: RATT-COPY! ( ptr u8 n ptr u8 ptr n -- ) {: a:ptr u dst:ptr lenp:ptr :}
   u FS-PATH-CAP > if E-RA-CAPACITY throw then
   a dst u BYTE-COPY
   u lenp ! ;

: RATT-ROOT$ ( -- ptr u8 n )
   RATT-ROOT RATT-ROOT-U @ ;

: RATT-PATH$ ( -- ptr u8 n )
   RATT-PATH RATT-PATH-U @ ;

: RATT-DIR$ ( -- ptr u8 n )
   RATT-DIR RATT-DIR-U @ ;

: RATT-REF$ ( -- ptr u8 n )
   RATT-REF RATT-REF-U @ ;

: RATT-CAND$ ( -- ptr u8 n )
   RATT-CAND RATT-CAND-U @ ;

: RATT-TESTS$ ( -- ptr u8 n )
   RATT-TESTS RATT-TESTS-U @ ;

: RATT-EXP$ ( -- ptr u8 n )
   RATT-EXP RATT-EXP-U @ ;

: RATT-PATH! ( ptr u8 n -- ) {: name:ptr nameu :}
   RATT-ROOT$ name nameu RATT-PATH JOIN-PATH RATT-PATH-U ! ;

: RATT-DIR! ( ptr u8 n -- ) {: name:ptr nameu :}
   RATT-ROOT$ name nameu RATT-DIR JOIN-PATH RATT-DIR-U ! ;

: RATT-DIR-PATH! ( ptr u8 n -- ) {: name:ptr nameu :}
   RATT-DIR$ name nameu RATT-PATH JOIN-PATH RATT-PATH-U ! ;

: RATT-REF-FILE! ( ptr u8 n -- ) {: name:ptr nameu :}
   RATT-REF$ name nameu RATT-PATH JOIN-PATH RATT-PATH-U ! ;

: RATT-WRITE-PATH ( -- )
   RATT-PATH$ s" : CAND ( -- ) ;" WRITE-ALL ;

: RATT-WRITE-ROOT ( ptr u8 n -- )
   RATT-PATH!
   RATT-WRITE-PATH ;

: RATT-WRITE-IN-DIR ( ptr u8 n -- )
   RATT-DIR-PATH!
   RATT-WRITE-PATH ;

: RATT-WRITE-REF ( ptr u8 n ptr u8 n -- ) {: name:ptr nameu text:ptr textu :}
   name nameu RATT-REF-FILE!
   RATT-PATH$ text textu WRITE-ALL ;

: RATT-WRITE-CAND ( ptr u8 n -- ) {: text:ptr textu :}
   RATT-CAND$ text textu WRITE-ALL ;

: RATT-WRITE-NAMED-CAND ( ptr u8 n ptr u8 n -- ) {: name:ptr nameu text:ptr textu :}
   name nameu RATT-PATH!
   RATT-PATH$ text textu WRITE-ALL ;

: RATT-WRITE-NAMED-IN-DIR ( ptr u8 n ptr u8 n -- ) {: name:ptr nameu text:ptr textu :}
   name nameu RATT-DIR-PATH!
   RATT-PATH$ text textu WRITE-ALL ;

: RATT-WRITE-TESTS ( ptr u8 n -- ) {: text:ptr textu :}
   RATT-TESTS$ text textu WRITE-ALL ;

: RATT-MAKE-DIR ( ptr u8 n -- )
   RATT-DIR!
   RATT-DIR$ MAKE-DIR ;

: RATT-EXP-ROOM ( n -- ) {: add :}
   add 0 < if E-RA-CAPACITY throw then
   add RA-SRC-CAP RATT-EXP-U @ - > if E-RA-CAPACITY throw then ;

: RATT-EXP+ ( ptr u8 n -- ) {: a:ptr u :}
   u RATT-EXP-ROOM
   a RATT-EXP RATT-EXP-U @ + u BYTE-COPY
   RATT-EXP-U @ u + RATT-EXP-U ! ;

: RATT-EXP-C ( n -- ) {: c :}
   1 RATT-EXP-ROOM
   c RATT-EXP RATT-EXP-U @ + c!
   RATT-EXP-U @ 1+ RATT-EXP-U ! ;

: RATT-EXP-LN ( ptr u8 n -- )
   RATT-EXP+
   STR-LF RATT-EXP-C ;

: RATT-JGET ( n ptr u8 n -- n ) {: root key:ptr keyu :}
   root key keyu JSON-GET dup -1 = if E-RA-MISSING throw then ;

: RATT-U-FIELD= ( n ptr u8 n n -- ) {: root key:ptr keyu want :}
   root key keyu RATT-JGET JSON-NUMBER$ STR>NUMBER? TTRUE
   want T= ;

: RATT-S-FIELD= ( n ptr u8 n ptr u8 n -- ) {: root key:ptr keyu want:ptr wantu :}
   root key keyu RATT-JGET JSON-STRING$ want wantu T$= ;

: RATT-BOOL-FIELD= ( n ptr u8 n bool -- ) {: root key:ptr keyu want :}
   root key keyu RATT-JGET JSON-BOOL@ if
      want TTRUE
   else
      want TFALSE
   then ;

: RATT-ARRAY-FIELD-COUNT= ( n ptr u8 n n -- ) {: root key:ptr keyu want :}
   root key keyu RATT-JGET dup JSON-KIND J-ARR T=
   JSON-COUNT want T= ;

: RATT-ARRAY-FIRST-FIELD ( n ptr u8 n -- n )
   RATT-JGET 0 JSON-ARR@ ;

: RATT-EXPECT-DIAG-QUALITY ( n -- ) {: root :}
   root s" diagnostic_token" RA-TRUE RATT-BOOL-FIELD=
   root s" diagnostic_span" RA-TRUE RATT-BOOL-FIELD=
   root s" diagnostic_expected" RA-TRUE RATT-BOOL-FIELD=
   root s" diagnostic_actual" RA-TRUE RATT-BOOL-FIELD=
   root s" diagnostic_code" RA-TRUE RATT-BOOL-FIELD=
   root s" diagnostic_repair_class" RA-TRUE RATT-BOOL-FIELD=
   root s" all_errors_stable" RA-TRUE RATT-BOOL-FIELD= ;

: RATT-PREPARE ( -- )
   CLEANUP-RESET
   s" habu-run-attempts" TMPDIR-MKDIR RATT-ROOT RATT-ROOT-U RATT-COPY!
   RATT-ROOT$ CLEANUP-TREE+ ;

: RATT-BUNDLE-PATHS! ( -- )
   RATT-ROOT$ s" ref" RATT-REF JOIN-PATH RATT-REF-U !
   RATT-ROOT$ s" cand.f" RATT-CAND JOIN-PATH RATT-CAND-U !
   RATT-ROOT$ s" tests.f" RATT-TESTS JOIN-PATH RATT-TESTS-U ! ;

: RATT-BUNDLE-TASKS$ ( -- ptr u8 n )
   s" 1	ONE	(-- i64)	arithmetic	-	forth	stack	-	-	v1	-	-
2	TWO	(-- i64)	arithmetic	-	forth	stack	-	-	v1	-	-
" ;

: RATT-ONE-SRC$ ( -- ptr u8 n )
   s" : ONE ( -- i64 ) 1 ;" ;

: RATT-TWO-SRC$ ( -- ptr u8 n )
   s" : TWO ( -- i64 ) 2 ;" ;

: RATT-CAND-SRC$ ( -- ptr u8 n )
   s" : TWO ( -- i64 ) 22 ;" ;

: RATT-ONE-GOOD-SRC$ ( -- ptr u8 n )
   s" : ONE ( -- i64 ) 1 ;" ;

: RATT-ONE-BAD-SRC$ ( -- ptr u8 n )
   s" : ONE ( -- i64 ) 1 2 ;" ;

: RATT-GOOD-CHECK-SRC$ ( -- ptr u8 n )
   s" : GOOD ( -- i64 ) 1 ;" ;

: RATT-BAD-CHECK-SRC$ ( -- ptr u8 n )
   s" : BAD ( i64 -- i64 ) dup ;" ;

: RATT-SIG-OK-SRC$ ( -- ptr u8 n )
   s" : SWAP2 ( n n -- n n ) swap ;" ;

: RATT-SIG-WEAK-SRC$ ( -- ptr u8 n )
   s" : SWAP2 ( n n -- n ) swap ;" ;

: RATT-TESTS-SRC$ ( -- ptr u8 n )
   s" ONE drop TWO drop 111 emit 107 emit" ;

: RATT-ONE-PASS-TESTS$ ( -- ptr u8 n )
   s" ONE drop 111 emit 107 emit" ;

: RATT-ONE-FAIL-TESTS$ ( -- ptr u8 n )
   s" ONE drop 111 emit" ;

: RATT-TASK21$ ( -- ptr u8 n )
   s" 21	ONE	(-- i64)	arithmetic	-	forth	stack	-	-	v1	-	-" ;

: RATT-TASK22$ ( -- ptr u8 n )
   s" 22	ONE	(-- i64)	arithmetic	-	forth	stack	-	-	v1	-	-" ;

: RATT-TASK23$ ( -- ptr u8 n )
   s" 23	ONE	(-- i64)	arithmetic	-	forth	stack	-	-	v1	-	-" ;

: RATT-TASK24$ ( -- ptr u8 n )
   s" 24	ONE	(-- i64)	arithmetic	-	forth	stack	-	-	v1	-	-" ;

: RATT-TASK25$ ( -- ptr u8 n )
   s" 25	ONE	(-- i64)	arithmetic	-	forth	stack	-	-	v1	-	-" ;

: RATT-PREPARE-BUNDLE-FIXTURE ( -- )
   RATT-BUNDLE-PATHS!
   RATT-REF$ MAKE-DIRS
   s" 1.f" RATT-ONE-SRC$ RATT-WRITE-REF
   s" 2.f" RATT-TWO-SRC$ RATT-WRITE-REF
   RATT-CAND-SRC$ RATT-WRITE-CAND
   RATT-TESTS-SRC$ RATT-WRITE-TESTS ;

: RATT-EXPECTED-BUNDLE! ( -- )
   0 RATT-EXP-U !
   RATT-ONE-SRC$ RATT-EXP-LN
   RATT-CAND-SRC$ RATT-EXP-LN
   RATT-TESTS-SRC$ RATT-EXP+ ;

: RATT-EXPECT-ROUND ( n ptr u8 n -- ) {: idx name:ptr nameu :}
   name nameu RATT-DIR-PATH!
   idx RA-ROUND$ RATT-PATH$ T$= ;

: RATT-EXPECT-SINGLE ( -- )
   s" 7.f" RATT-WRITE-ROOT
   RATT-ROOT$ s" 7" RA-CANDIDATES 1 T=
   s" 7.f" RATT-PATH!
   0 RA-ROUND$ RATT-PATH$ T$= ;

: RATT-EXPECT-MULTI-ROUND ( -- )
   s" 8" RATT-MAKE-DIR
   s" 10.f" RATT-WRITE-IN-DIR
   s" 1.f" RATT-WRITE-IN-DIR
   s" 2.f" RATT-WRITE-IN-DIR
   RATT-ROOT$ s" 8" RA-CANDIDATES 3 T=
   0 s" 1.f" RATT-EXPECT-ROUND
   1 s" 2.f" RATT-EXPECT-ROUND
   2 s" 10.f" RATT-EXPECT-ROUND ;

: RATT-EXPECT-DIR-PRECEDENCE ( -- )
   s" 11.f" RATT-WRITE-ROOT
   s" 11" RATT-MAKE-DIR
   s" 1.f" RATT-WRITE-IN-DIR
   RATT-ROOT$ s" 11" RA-CANDIDATES 1 T=
   0 s" 1.f" RATT-EXPECT-ROUND ;

: RATT-EXPECT-MISSING ( -- )
   RATT-ROOT$ s" missing" RA-CANDIDATES drop ;

: RATT-EXPECT-EMPTY-DIR ( -- )
   s" 9" RATT-MAKE-DIR
   RATT-ROOT$ s" 9" RA-CANDIDATES drop ;

: RATT-EXPECT-BAD-ROUND ( -- )
   RA-ROUND-MAX RA-ROUND$ 2drop ;

: RATT-EXPECT-BUNDLE ( -- )
   RATT-PREPARE-BUNDLE-FIXTURE
   RATT-EXPECTED-BUNDLE!
   RATT-BUNDLE-TASKS$ RATT-REF$ s" 2" RATT-CAND$ RATT-TESTS$ RA-BUILD-BUNDLE
   RATT-EXP$ T$= ;

: RATT-EXPECT-MISSING-CANDIDATE ( -- )
   s" missing-cand.f" RATT-PATH!
   RATT-BUNDLE-TASKS$ RATT-REF$ s" 2" RATT-PATH$ RATT-TESTS$ RA-BUILD-BUNDLE 2drop ;

: RATT-EXPECT-MISSING-REF ( -- )
   s" no-ref" RATT-DIR!
   RATT-BUNDLE-TASKS$ RATT-DIR$ s" 1" RATT-CAND$ RATT-TESTS$ RA-BUILD-BUNDLE 2drop ;

: RATT-EXPECT-MISSING-TARGET ( -- )
   RATT-BUNDLE-TASKS$ RATT-REF$ s" 99" RATT-CAND$ RATT-TESTS$ RA-BUILD-BUNDLE 2drop ;

: RATT-EXPECT-BUNDLE-CAPACITY ( -- )
   RA-BUNDLE-CAP 1+ RA-BUNDLE-ROOM ;

: RATT-EXPECT-CHECK-PASS ( -- )
   RATT-GOOD-CHECK-SRC$ RATT-WRITE-CAND
   RATT-CAND$ RA-CHECK-CANDIDATE TTRUE
   RA-RC@ 0 T=
   RA-OUT$ s" " T$=
   RA-ERR$ s" " T$= ;

: RATT-EXPECT-CHECK-REJECT ( -- )
   RATT-BAD-CHECK-SRC$ RATT-WRITE-CAND
   RATT-CAND$ RA-CHECK-CANDIDATE TFALSE
   RA-RC@ 0 <> TTRUE
   RA-OUT$ s" " T$=
   RA-ERR$ s" E-MISMATCH" CONTAINS? TTRUE
   RA-ERR$ s" repair_class" CONTAINS? TTRUE ;

: RATT-EXPECT-RUN-TESTS-PASS ( -- )
   RATT-PREPARE-BUNDLE-FIXTURE
   RATT-BUNDLE-TASKS$ RATT-REF$ s" 2" RATT-CAND$ RATT-TESTS$
   RA-RUN-CANDIDATE-TESTS TTRUE
   RA-RC@ 0 T=
   RA-OUT$ TRIM s" ok" T$=
   RA-ERR$ s" " T$= ;

: RATT-EXPECT-RUN-TESTS-FAIL ( -- )
   s" 111 emit" RA-RUN-BUNDLE-TESTS TFALSE
   RA-RC@ 0 T=
   RA-OUT$ s" o" T$= ;

: RATT-EXPECT-RUN-TESTS-TRUNCATED ( -- )
   s" : SPAM ( -- ) 20000 0 ?do 65 emit loop ; SPAM" RA-RUN-BUNDLE-TESTS drop ;

: RATT-EXPECT-RUN-TESTS-TIMEOUT ( -- )
   50 RA-TIMEOUT-MS !
   s" : HANG ( -- ) begin again ; HANG" RA-RUN-BUNDLE-TESTS drop ;

: RATT-WRITE-TRUST-SRC ( -- )
   SB-RESET
   s" TRUST" SB-APPEND
   STR-LF SB-APPEND-C
   s" untrusted" SB-APPEND
   STR-LF SB-APPEND-C
   SB$ RATT-WRITE-CAND ;

: RATT-EXPECT-METRIC-CERTIFIED ( -- )
   RA-ROW-RESET
   RATT-GOOD-CHECK-SRC$ RATT-WRITE-CAND
   RA-CHECKER++
   RATT-CAND$ RA-CHECK-CANDIDATE TTRUE
   RATT-CAND$ RA-RECORD-CERTIFIED
   RATT-CAND$ RA-RECORD-TEST-PASS
   RA-CHECKERS 1 T=
   RA-REPAIRS 0 T=
   RA-FIRST-CHECKER$ s" certified" T$=
   RA-FIRST-TESTS? TTRUE
   RA-TESTS-PASSED? TTRUE ;

: RATT-EXPECT-METRIC-REJECT ( -- )
   RA-ROW-RESET
   RATT-BAD-CHECK-SRC$ RATT-WRITE-CAND
   RA-CHECKER++
   RATT-CAND$ RA-CHECK-CANDIDATE TFALSE
   RATT-CAND$ 1 RA-RECORD-REJECT
   RA-CHECKERS 1 T=
   RA-FIRST-CHECKER$ s" rejected" T$=
   RA-DIAGNOSTIC-COUNT 1 T=
   RA-DIAGNOSTIC-REPAIR-CLASS? TTRUE
   RA-DIAGS$ s" E-MISMATCH" CONTAINS? TTRUE
   RA-EVENTS$ s" remove_producer" CONTAINS? TTRUE
   RA-REPAIR-STATS$ s" repair_class" CONTAINS? TTRUE ;

: RATT-EXPECT-FINAL-METRICS ( -- )
   RATT-SIG-OK-SRC$ RATT-WRITE-CAND
   RA-ROW-RESET
   RATT-CAND$ RA-SET-FINAL
   s" n n -- n n" RA-FINAL-METRICS!
   RA-FINAL-CHARS RATT-SIG-OK-SRC$ nip T=
   RA-TRUST-USES 0 T=
   RA-SIGNATURE-WEAKENED? TFALSE
   RATT-SIG-WEAK-SRC$ RATT-WRITE-CAND
   RATT-CAND$ RA-SET-FINAL
   s" n n -- n n" RA-FINAL-METRICS!
   RA-SIGNATURE-WEAKENED? TTRUE
   RATT-WRITE-TRUST-SRC
   RATT-CAND$ RA-SET-FINAL
   s" --" RA-FINAL-METRICS!
   RA-TRUST-USES 1 T= ;

: RATT-PREPARE-CERTIFIED-ROW ( -- )
   RA-ROW-RESET
   RATT-GOOD-CHECK-SRC$ RATT-WRITE-CAND
   RA-CHECKER++
   RATT-CAND$ RA-CHECK-CANDIDATE TTRUE
   RATT-CAND$ RA-RECORD-CERTIFIED
   RATT-CAND$ RA-RECORD-TEST-PASS
   s" -- i64" RA-FINAL-METRICS!
   RA-ALL-ERRORS-STABLE! ;

: RATT-PREPARE-REPAIRED-ROW ( -- )
   RA-ROW-RESET
   s" bad-row.f" RATT-BAD-CHECK-SRC$ RATT-WRITE-NAMED-CAND
   RA-CHECKER++
   RATT-PATH$ RA-CHECK-CANDIDATE TFALSE
   RATT-PATH$ 1 RA-RECORD-REJECT
   RATT-GOOD-CHECK-SRC$ RATT-WRITE-CAND
   RA-CHECKER++
   RATT-CAND$ RA-CHECK-CANDIDATE TTRUE
   RATT-CAND$ RA-RECORD-CERTIFIED
   RATT-CAND$ RA-RECORD-TEST-PASS
   s" -- i64" RA-FINAL-METRICS!
   RA-ALL-ERRORS-STABLE! ;

: RATT-EXPECT-CERTIFIED-ROW ( -- )
   RATT-PREPARE-CERTIFIED-ROW
   s" attempt-fixture" 1 s" GOOD" s" fixture-model" 17 RA-ROW$ JSON-PARSE {: root :}
   root s" schema_version" 1 RATT-U-FIELD=
   root s" run_id" s" attempt-fixture" RATT-S-FIELD=
   root s" task_id" 1 RATT-U-FIELD=
   root s" name" s" GOOD" RATT-S-FIELD=
   root s" model" s" fixture-model" RATT-S-FIELD=
   root s" attempt" 1 RATT-U-FIELD=
   root s" first_pass_checker" s" certified" RATT-S-FIELD=
   root s" first_pass_tests" RA-TRUE RATT-BOOL-FIELD=
   root s" tests_passed" RA-TRUE RATT-BOOL-FIELD=
   root s" repair_iterations" 0 RATT-U-FIELD=
   root s" checker_iterations" 1 RATT-U-FIELD=
   root s" diagnostic_count" 0 RATT-U-FIELD=
   root RATT-EXPECT-DIAG-QUALITY
   root s" repair_class_stats" 0 RATT-ARRAY-FIELD-COUNT=
   root s" tokens_used" 0 RATT-U-FIELD=
   root s" wall_ms" 17 RATT-U-FIELD=
   root s" final_chars" RATT-GOOD-CHECK-SRC$ nip RATT-U-FIELD=
   root s" trust_uses" 0 RATT-U-FIELD=
   root s" signature_weakened" RA-FALSE RATT-BOOL-FIELD= ;

: RATT-EXPECT-REPAIRED-ROW ( -- )
   RATT-PREPARE-REPAIRED-ROW
   s" repair-fixture" 9 s" BAD" s" fixture-model" 29 RA-ROW$ JSON-PARSE {: root :}
   root s" attempt" 2 RATT-U-FIELD=
   root s" first_pass_checker" s" rejected" RATT-S-FIELD=
   root s" first_pass_tests" RA-FALSE RATT-BOOL-FIELD=
   root s" tests_passed" RA-TRUE RATT-BOOL-FIELD=
   root s" repair_iterations" 1 RATT-U-FIELD=
   root s" checker_iterations" 2 RATT-U-FIELD=
   root s" diagnostic_count" 1 RATT-U-FIELD=
   root RATT-EXPECT-DIAG-QUALITY
   root s" repair_class_stats" 1 RATT-ARRAY-FIELD-COUNT=
   root s" repair_class_stats" RATT-ARRAY-FIRST-FIELD {: stat :}
   stat s" repair_class" s" remove_producer" RATT-S-FIELD=
   stat s" diagnostic_count" 1 RATT-U-FIELD=
   stat s" repair_success" RA-TRUE RATT-BOOL-FIELD=
   stat s" repair_iterations" 1 RATT-U-FIELD=
   stat s" token_delta" 0 RATT-U-FIELD=
   root s" wall_ms" 29 RATT-U-FIELD=
   root s" final_chars" RATT-GOOD-CHECK-SRC$ nip RATT-U-FIELD=
   root s" signature_weakened" RA-FALSE RATT-BOOL-FIELD= ;

: RATT-PREPARE-TASK-LOOP ( ptr u8 n -- ) {: tests:ptr testsu :}
   RATT-BUNDLE-PATHS!
   RATT-REF$ MAKE-DIRS
   tests testsu RATT-WRITE-TESTS ;

: RATT-RUN-TASK-ROW ( ptr u8 n -- n ) {: task:ptr tasku :}
   task tasku RATT-REF$ RATT-ROOT$ RATT-TESTS$
   s" attempt-loop" s" fixture-model" RA-RUN-TASKS JSON-PARSE ;

: RATT-EXPECT-TASK-FIRST-PASS ( -- )
   RATT-ONE-PASS-TESTS$ RATT-PREPARE-TASK-LOOP
   s" 21.f" RATT-ONE-GOOD-SRC$ RATT-WRITE-NAMED-CAND
   RATT-TASK21$ RATT-RUN-TASK-ROW {: root :}
   root s" task_id" 21 RATT-U-FIELD=
   root s" attempt" 1 RATT-U-FIELD=
   root s" first_pass_checker" s" certified" RATT-S-FIELD=
   root s" first_pass_tests" RA-TRUE RATT-BOOL-FIELD=
   root s" tests_passed" RA-TRUE RATT-BOOL-FIELD=
   root s" diagnostic_count" 0 RATT-U-FIELD= ;

: RATT-EXPECT-TASK-REPAIRED ( -- )
   RATT-ONE-PASS-TESTS$ RATT-PREPARE-TASK-LOOP
   s" 25" RATT-MAKE-DIR
   s" 10.f" RATT-ONE-GOOD-SRC$ RATT-WRITE-NAMED-IN-DIR
   s" 1.f" RATT-ONE-BAD-SRC$ RATT-WRITE-NAMED-IN-DIR
   s" 2.f" RATT-ONE-GOOD-SRC$ RATT-WRITE-NAMED-IN-DIR
   RATT-TASK25$ RATT-RUN-TASK-ROW {: root :}
   root s" task_id" 25 RATT-U-FIELD=
   root s" attempt" 2 RATT-U-FIELD=
   root s" first_pass_checker" s" rejected" RATT-S-FIELD=
   root s" tests_passed" RA-TRUE RATT-BOOL-FIELD=
   root s" repair_iterations" 1 RATT-U-FIELD=
   root s" checker_iterations" 2 RATT-U-FIELD=
   root s" repair_class_stats" 1 RATT-ARRAY-FIELD-COUNT= ;

: RATT-EXPECT-TASK-REJECT ( -- )
   RATT-ONE-PASS-TESTS$ RATT-PREPARE-TASK-LOOP
   s" 22.f" RATT-ONE-BAD-SRC$ RATT-WRITE-NAMED-CAND
   RATT-TASK22$ RATT-RUN-TASK-ROW {: root :}
   root s" attempt" 1 RATT-U-FIELD=
   root s" first_pass_checker" s" rejected" RATT-S-FIELD=
   root s" first_pass_tests" RA-FALSE RATT-BOOL-FIELD=
   root s" tests_passed" RA-FALSE RATT-BOOL-FIELD=
   root s" diagnostic_count" 1 RATT-U-FIELD= ;

: RATT-EXPECT-TASK-TEST-FAIL ( -- )
   RATT-ONE-FAIL-TESTS$ RATT-PREPARE-TASK-LOOP
   s" 23.f" RATT-ONE-GOOD-SRC$ RATT-WRITE-NAMED-CAND
   RATT-TASK23$ RATT-RUN-TASK-ROW {: root :}
   root s" attempt" 1 RATT-U-FIELD=
   root s" first_pass_checker" s" certified" RATT-S-FIELD=
   root s" first_pass_tests" RA-FALSE RATT-BOOL-FIELD=
   root s" tests_passed" RA-FALSE RATT-BOOL-FIELD=
   root s" diagnostic_count" 0 RATT-U-FIELD= ;

: RATT-EXPECT-TASK-MISSING ( -- )
   RATT-ONE-PASS-TESTS$ RATT-PREPARE-TASK-LOOP
   RATT-TASK24$ RA-JSONL-RESET RATT-RUN-TASK-ROW drop ;

: RATT-MAIN ( -- )
   T-RESET
   RATT-PREPARE
   RATT-EXPECT-SINGLE
   RATT-EXPECT-MULTI-ROUND
   RATT-EXPECT-DIR-PRECEDENCE
   RATT-EXPECT-BUNDLE
   ['] RATT-EXPECT-MISSING E-RA-MISSING TTHROWS
   ['] RATT-EXPECT-EMPTY-DIR E-RA-MISSING TTHROWS
   ['] RATT-EXPECT-BAD-ROUND E-RA-CAPACITY TTHROWS
   ['] RATT-EXPECT-MISSING-CANDIDATE E-RA-MISSING TTHROWS
   ['] RATT-EXPECT-MISSING-REF E-RA-MISSING TTHROWS
   ['] RATT-EXPECT-MISSING-TARGET E-RA-MISSING TTHROWS
   ['] RATT-EXPECT-BUNDLE-CAPACITY E-RA-CAPACITY TTHROWS
   RATT-EXPECT-CHECK-PASS
   RATT-EXPECT-CHECK-REJECT
   RATT-EXPECT-RUN-TESTS-PASS
   RATT-EXPECT-RUN-TESTS-FAIL
   ['] RATT-EXPECT-RUN-TESTS-TRUNCATED E-PROC-TRUNCATED TTHROWS
   ['] RATT-EXPECT-RUN-TESTS-TIMEOUT E-PROC-TIMEOUT TTHROWS
   RA-DEFAULT-TIMEOUT!
   RATT-EXPECT-METRIC-CERTIFIED
   RATT-EXPECT-METRIC-REJECT
   RATT-EXPECT-FINAL-METRICS
   RATT-EXPECT-CERTIFIED-ROW
   RATT-EXPECT-REPAIRED-ROW
   RATT-EXPECT-TASK-FIRST-PASS
   RATT-EXPECT-TASK-REPAIRED
   RATT-EXPECT-TASK-REJECT
   RATT-EXPECT-TASK-TEST-FAIL
   ['] RATT-EXPECT-TASK-MISSING E-RA-MISSING TTHROWS
   CLEANUP-RUN
   RATT-ROOT$ EXISTS? TFALSE
   T-REPORT
   s" run-attempts-test: ok" type cr ;

RATT-MAIN
