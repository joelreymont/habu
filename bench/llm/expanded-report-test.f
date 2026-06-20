\ expanded-report-test.f - focused tests for expanded benchmark reports.

65536 constant ERT-CAPTURE-CAP
8192 constant ERT-JSON-CAP
2000 constant ERT-TIMEOUT-MS
10 constant ERT-LF
34 constant ERT-DQ
44 constant ERT-COMMA-C
58 constant ERT-COLON
91 constant ERT-LBRACK
93 constant ERT-RBRACK
123 constant ERT-LBRACE
125 constant ERT-RBRACE

create ERT-ROOT FS-PATH-CAP allot
create ERT-RESULT FS-PATH-CAP allot
create ERT-PERF FS-PATH-CAP allot
create ERT-JSON ERT-JSON-CAP allot
create ERT-OUT ERT-CAPTURE-CAP allot
create ERT-ERR ERT-CAPTURE-CAP allot
create ERT-LF-BUF 1 allot

variable ERT-ROOT-U
variable ERT-RESULT-U
variable ERT-PERF-U
variable ERT-JSON-U
variable ERT-OUT-U

ERT-LF ERT-LF-BUF c!

: ERT-COPY! ( ptr u8 n ptr u8 ptr n -- ) {: a:ptr u dst:ptr lenp:ptr :}
   a dst u BYTE-COPY
   u lenp ! ;

: ERT-ROOT$ ( -- ptr u8 n )
   ERT-ROOT ERT-ROOT-U @ ;

: ERT-RESULT$ ( -- ptr u8 n )
   ERT-RESULT ERT-RESULT-U @ ;

: ERT-PERF$ ( -- ptr u8 n )
   ERT-PERF ERT-PERF-U @ ;

: ERT-HASH$ ( -- ptr u8 n )
   s" 0000000000000000000000000000000000000000000000000000000000000000" ;

: ERT-J-RESET ( -- )
   0 ERT-JSON-U ! ;

: ERT-COMMA ( -- )
   ERT-JSON-U @ 1+ ERT-JSON-CAP > if E-FS-CAPACITY throw then
   ERT-COMMA-C ERT-JSON ERT-JSON-U @ + c!
   ERT-JSON-U @ 1+ ERT-JSON-U ! ;

: ERT-J-C ( n -- ) {: c :}
   ERT-JSON-U @ 1+ ERT-JSON-CAP > if E-FS-CAPACITY throw then
   c ERT-JSON ERT-JSON-U @ + c!
   ERT-JSON-U @ 1+ ERT-JSON-U ! ;

: ERT-J+ ( ptr u8 n -- ) {: a:ptr u :}
   ERT-JSON-U @ u + ERT-JSON-CAP > if E-FS-CAPACITY throw then
   a ERT-JSON ERT-JSON-U @ + u BYTE-COPY
   ERT-JSON-U @ u + ERT-JSON-U ! ;

: ERT-J-Q ( -- )
   ERT-DQ ERT-J-C ;

: ERT-J-KEY ( ptr u8 n -- )
   ERT-J-Q ERT-J+ ERT-J-Q ERT-COLON ERT-J-C ;

: ERT-J-STRING ( ptr u8 n -- )
   ERT-J-Q ERT-J+ ERT-J-Q ;

: ERT-FIELD-U-LAST ( ptr u8 n ptr u8 n -- ) {: key:ptr keyu val:ptr valu :}
   key keyu ERT-J-KEY
   val valu ERT-J+ ;

: ERT-FIELD-S-LAST ( ptr u8 n ptr u8 n -- ) {: key:ptr keyu val:ptr valu :}
   key keyu ERT-J-KEY
   val valu ERT-J-STRING ;

: ERT-FIELD-U ( ptr u8 n ptr u8 n -- )
   ERT-FIELD-U-LAST
   ERT-COMMA ;

: ERT-FIELD-S ( ptr u8 n ptr u8 n -- )
   ERT-FIELD-S-LAST
   ERT-COMMA ;

: ERT-FIELD-HASH ( ptr u8 n -- )
   ERT-HASH$ ERT-FIELD-S ;

: ERT-WRITE-ROW ( -- )
   ERT-J-RESET
   ERT-LBRACE ERT-J-C
   s" schema_version" s" 2" ERT-FIELD-U
   s" run_id" s" fixture-2026-06-20" ERT-FIELD-S
   s" task_id" s" 1" ERT-FIELD-U
   s" name" s" SQUARE" ERT-FIELD-S
   s" model_id" s" fixture" ERT-FIELD-S
   s" model" s" Fixture" ERT-FIELD-S
   s" model_version" s" unknown" ERT-FIELD-S
   s" model_date" s" unknown" ERT-FIELD-S
   s" arm" s" habu-forth" ERT-FIELD-S
   s" trial_id" s" fixture:fixture:habu-forth:1:1" ERT-FIELD-S
   s" trial" s" 1" ERT-FIELD-U
   s" task_order" s" 1" ERT-FIELD-U
   s" k_trials" s" 1" ERT-FIELD-U
   s" order_seed" s" fixture" ERT-FIELD-S
   s" task_family" s" arithmetic" ERT-FIELD-S
   s" outcome" s" pass" ERT-FIELD-S
   s" rounds" s" 1" ERT-FIELD-U
   s" first_pass" s" true" ERT-FIELD-U
   s" tokens" s" 9" ERT-FIELD-U
   s" source_chars" s" 31" ERT-FIELD-U
   s" runtime_ms" s" 2" ERT-FIELD-U
   s" runtime_repetitions" s" 1" ERT-FIELD-U
   s" runtime_warmups" s" 0" ERT-FIELD-U
   s" runtime_status" s" ok" ERT-FIELD-S
   s" attempt" s" 1" ERT-FIELD-U
   s" first_pass_checker" s" certified" ERT-FIELD-S
   s" first_pass_tests" s" true" ERT-FIELD-U
   s" tests_passed" s" true" ERT-FIELD-U
   s" repair_iterations" s" 0" ERT-FIELD-U
   s" checker_iterations" s" 1" ERT-FIELD-U
   s" diagnostic_count" s" 0" ERT-FIELD-U
   s" diagnostic_token" s" true" ERT-FIELD-U
   s" diagnostic_span" s" true" ERT-FIELD-U
   s" diagnostic_expected" s" true" ERT-FIELD-U
   s" diagnostic_actual" s" true" ERT-FIELD-U
   s" diagnostic_code" s" true" ERT-FIELD-U
   s" diagnostic_repair_class" s" true" ERT-FIELD-U
   s" all_errors_stable" s" true" ERT-FIELD-U
   s" tokens_used" s" 9" ERT-FIELD-U
   s" wall_ms" s" 12" ERT-FIELD-U
   s" final_chars" s" 31" ERT-FIELD-U
   s" trust_uses" s" 0" ERT-FIELD-U
   s" signature_weakened" s" false" ERT-FIELD-U
   s" prompt" s" Define SQUARE." ERT-FIELD-S
   s" prompt_sha256" ERT-FIELD-HASH
   s" raw_response" s" : SQUARE ( i64 -- i64 ) dup * ;" ERT-FIELD-S
   s" raw_response_sha256" ERT-FIELD-HASH
   s" extracted_candidate" s" : SQUARE ( i64 -- i64 ) dup * ;" ERT-FIELD-S
   s" extracted_candidate_sha256" ERT-FIELD-HASH
   s" checker_diagnostics" s" " ERT-FIELD-S
   s" checker_diagnostics_sha256" ERT-FIELD-HASH
   s" repair_packet" s" " ERT-FIELD-S
   s" repair_packet_sha256" ERT-FIELD-HASH
   s" test_output" s" ok" ERT-FIELD-S
   s" test_output_sha256" ERT-FIELD-HASH
   s" final_bundle" s" : SQUARE ( i64 -- i64 ) dup * ;" ERT-FIELD-S
   s" final_bundle_sha256" ERT-HASH$ ERT-FIELD-S-LAST
   ERT-RBRACE ERT-J-C
   ERT-RESULT$ ERT-JSON ERT-JSON-U @ WRITE-ALL
   ERT-RESULT$ ERT-LF-BUF 1 APPEND-FILE ;

: ERT-WRITE-PERF ( -- )
   ERT-J-RESET
   ERT-LBRACE ERT-J-C
   s" schema_version" s" 1" ERT-FIELD-U
   s" bench" s" llm-perf" ERT-FIELD-S
   s" full" s" false" ERT-FIELD-U
   s" results" ERT-J-KEY ERT-LBRACK ERT-J-C
   ERT-LBRACE ERT-J-C
   s" name" s" check_solutions" ERT-FIELD-S
   s" wall_ms" s" 12" ERT-FIELD-U-LAST
   ERT-RBRACE ERT-J-C
   ERT-COMMA
   ERT-LBRACE ERT-J-C
   s" name" s" metric_validator" ERT-FIELD-S
   s" wall_ms" s" 34" ERT-FIELD-U-LAST
   ERT-RBRACE ERT-J-C
   ERT-COMMA
   ERT-LBRACE ERT-J-C
   s" name" s" microbench_smoke" ERT-FIELD-S
   s" wall_ms" s" 56" ERT-FIELD-U-LAST
   ERT-RBRACE ERT-J-C
   ERT-RBRACK ERT-J-C
   ERT-RBRACE ERT-J-C
   ERT-PERF$ ERT-JSON ERT-JSON-U @ WRITE-ALL ;

: ERT-PREPARE ( -- )
   CLEANUP-RESET
   s" habu-expanded-report" TMPDIR-MKDIR ERT-ROOT ERT-ROOT-U ERT-COPY!
   ERT-ROOT$ CLEANUP-TREE+
   ERT-ROOT$ s" run.jsonl" ERT-RESULT JOIN-PATH ERT-RESULT-U !
   ERT-ROOT$ s" perf.json" ERT-PERF JOIN-PATH ERT-PERF-U !
   ERT-WRITE-ROW
   ERT-WRITE-PERF ;

: ERT-RUN-REPORT ( -- n n n )
   PROC-ARGV-RESET
   s" --load" PROC-ARGV+
   s" lib/errors.f" PROC-ARGV+
   s" lib/string.f" PROC-ARGV+
   s" lib/fs.f" PROC-ARGV+
   s" lib/process.f" PROC-ARGV+
   s" lib/process-argv.f" PROC-ARGV+
   s" lib/time.f" PROC-ARGV+
   s" lib/date.f" PROC-ARGV+
   s" lib/argv.f" PROC-ARGV+
   s" tools/json.f" PROC-ARGV+
   s" bench/llm/expanded-report.f" PROC-ARGV+
   s" --" PROC-ARGV+
   ERT-RESULT$ PROC-ARGV+
   ERT-PERF$ PROC-ARGV+
   s" bin/hb" ERT-OUT ERT-CAPTURE-CAP ERT-ERR ERT-CAPTURE-CAP ERT-TIMEOUT-MS RUN-ARGV-CAPTURE ;

: ERT-CONTAINS ( ptr u8 n -- ) {: a:ptr u :}
   ERT-OUT ERT-OUT-U @ a u FIND-SUB 0 >= TTRUE ;

: ERT-TEST-LATENCY ( -- )
   ERT-RUN-REPORT {: outu erru rc :}
   rc 0 T=
   erru 0 T=
   outu 0 > TTRUE
   outu ERT-OUT-U !
   s" ## LLM Feedback Latency" ERT-CONTAINS
   s" bench/llm/perf.sh --json" ERT-CONTAINS
   s" | metric_validator | 34 | 0.03 |" ERT-CONTAINS
   s" | microbench_smoke | 56 | 0.06 |" ERT-CONTAINS ;

: ERT-MAIN ( -- )
   T-RESET
   ERT-PREPARE
   ERT-TEST-LATENCY
   CLEANUP-RUN
   T-REPORT
   s" expanded-report-test: ok" type cr ;

ERT-MAIN
