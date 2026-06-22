\ expanded-report-test.f - focused tests for expanded benchmark reports.

65536 constant ERT-CAPTURE-CAP
2000 constant ERT-TIMEOUT-MS
10 constant ERT-LF

create ERT-ROOT FS-PATH-CAP allot
create ERT-RESULT FS-PATH-CAP allot
create ERT-PERF FS-PATH-CAP allot
create ERT-OUT ERT-CAPTURE-CAP allot
create ERT-OUT2 ERT-CAPTURE-CAP allot
create ERT-ERR ERT-CAPTURE-CAP allot
create ERT-LF-BUF 1 allot

variable ERT-ROOT-U
variable ERT-RESULT-U
variable ERT-PERF-U
variable ERT-OUT-U
variable ERT-OUT2-U

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

: ERT-TRUE ( -- bool )
   0 0= ;

: ERT-FALSE ( -- bool )
   ERT-TRUE 0= ;

: ERT-FIELD-U-LAST ( ptr u8 n n -- )
   JW-FIELD-U ;

: ERT-FIELD-S-LAST ( ptr u8 n ptr u8 n -- )
   JW-FIELD-S ;

: ERT-FIELD-BOOL-LAST ( ptr u8 n bool -- )
   JW-FIELD-BOOL ;

: ERT-FIELD-U ( ptr u8 n n -- )
   ERT-FIELD-U-LAST
   JW-COMMA ;

: ERT-FIELD-S ( ptr u8 n ptr u8 n -- )
   ERT-FIELD-S-LAST
   JW-COMMA ;

: ERT-FIELD-BOOL ( ptr u8 n bool -- )
   ERT-FIELD-BOOL-LAST
   JW-COMMA ;

: ERT-FIELD-HASH ( ptr u8 n -- )
   ERT-HASH$ ERT-FIELD-S ;

: ERT-ROW-BEGIN ( -- )
   JW-RESET
   JW-OBJECT-START
   s" schema_version" 2 ERT-FIELD-U
   s" run_id" s" fixture-2026-06-20" ERT-FIELD-S
   s" task_id" 1 ERT-FIELD-U
   s" name" s" SQUARE" ERT-FIELD-S ;

: ERT-ROW-MODEL ( -- )
   s" model_id" s" fixture" ERT-FIELD-S
   s" model" s" Fixture" ERT-FIELD-S
   s" model_version" s" unknown" ERT-FIELD-S
   s" model_date" s" unknown" ERT-FIELD-S
   s" arm" s" habu-forth" ERT-FIELD-S ;

: ERT-ROW-TRIAL ( -- )
   s" trial_id" s" fixture:fixture:habu-forth:1:1" ERT-FIELD-S
   s" trial" 1 ERT-FIELD-U
   s" task_order" 1 ERT-FIELD-U
   s" k_trials" 1 ERT-FIELD-U
   s" order_seed" s" fixture" ERT-FIELD-S
   s" task_family" s" arithmetic" ERT-FIELD-S ;

: ERT-ROW-OUTCOME ( -- )
   s" outcome" s" pass" ERT-FIELD-S
   s" rounds" 1 ERT-FIELD-U
   s" first_pass" ERT-TRUE ERT-FIELD-BOOL
   s" tokens" 9 ERT-FIELD-U
   s" source_chars" 31 ERT-FIELD-U
   s" runtime_ms" 2 ERT-FIELD-U
   s" runtime_repetitions" 1 ERT-FIELD-U
   s" runtime_warmups" 0 ERT-FIELD-U
   s" runtime_status" s" ok" ERT-FIELD-S
   s" attempt" 1 ERT-FIELD-U
   s" first_pass_checker" s" certified" ERT-FIELD-S
   s" checker_false_reject" ERT-FALSE ERT-FIELD-BOOL
   s" first_pass_tests" ERT-TRUE ERT-FIELD-BOOL
   s" tests_passed" ERT-TRUE ERT-FIELD-BOOL
   s" repair_iterations" 0 ERT-FIELD-U
   s" checker_iterations" 1 ERT-FIELD-U
   s" diagnostic_count" 0 ERT-FIELD-U ;

: ERT-ROW-DIAGNOSTICS ( -- )
   s" diagnostic_token" ERT-TRUE ERT-FIELD-BOOL
   s" diagnostic_span" ERT-TRUE ERT-FIELD-BOOL
   s" diagnostic_expected" ERT-TRUE ERT-FIELD-BOOL
   s" diagnostic_actual" ERT-TRUE ERT-FIELD-BOOL
   s" diagnostic_code" ERT-TRUE ERT-FIELD-BOOL
   s" diagnostic_repair_class" ERT-TRUE ERT-FIELD-BOOL
   s" all_errors_stable" ERT-TRUE ERT-FIELD-BOOL
   s" tokens_used" 9 ERT-FIELD-U
   s" wall_ms" 12 ERT-FIELD-U
   s" final_chars" 31 ERT-FIELD-U
   s" trust_uses" 0 ERT-FIELD-U
   s" signature_weakened" ERT-FALSE ERT-FIELD-BOOL ;

: ERT-ROW-ARTIFACTS ( -- )
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
   s" final_bundle_sha256" ERT-HASH$ ERT-FIELD-S-LAST ;

: ERT-ROW-END ( -- )
   JW-OBJECT-END
   ERT-RESULT$ JW$ WRITE-ALL
   ERT-RESULT$ ERT-LF-BUF 1 APPEND-FILE ;

: ERT-WRITE-ROW ( -- )
   ERT-ROW-BEGIN
   ERT-ROW-MODEL
   ERT-ROW-TRIAL
   ERT-ROW-OUTCOME
   ERT-ROW-DIAGNOSTICS
   ERT-ROW-ARTIFACTS
   ERT-ROW-END ;

: ERT-WRITE-PERF ( -- )
   JW-RESET
   JW-OBJECT-START
   s" schema_version" 1 ERT-FIELD-U
   s" bench" s" llm-perf" ERT-FIELD-S
   s" full" ERT-FALSE ERT-FIELD-BOOL
   s" results" JW-KEY JW-ARRAY-START
   JW-OBJECT-START
   s" name" s" check_solutions" ERT-FIELD-S
   s" wall_ms" 12 ERT-FIELD-U-LAST
   JW-OBJECT-END
   JW-COMMA
   JW-OBJECT-START
   s" name" s" metric_validator" ERT-FIELD-S
   s" wall_ms" 34 ERT-FIELD-U-LAST
   JW-OBJECT-END
   JW-COMMA
   JW-OBJECT-START
   s" name" s" microbench_smoke" ERT-FIELD-S
   s" wall_ms" 56 ERT-FIELD-U-LAST
   JW-OBJECT-END
   JW-ARRAY-END
   JW-OBJECT-END
   ERT-PERF$ JW$ WRITE-ALL ;

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
   s" --load"  >LEN PROC-ARGV+
   s" lib/errors.f"  >LEN PROC-ARGV+
   s" lib/string.f"  >LEN PROC-ARGV+
   s" lib/memory.f"  >LEN PROC-ARGV+
   s" lib/fs.f"  >LEN PROC-ARGV+
   s" lib/process.f"  >LEN PROC-ARGV+
   s" lib/process-argv.f"  >LEN PROC-ARGV+
   s" lib/argv.f"  >LEN PROC-ARGV+
   s" tools/date.f"  >LEN PROC-ARGV+
   s" tools/lint/text.f"  >LEN PROC-ARGV+
   s" tools/lint/token.f"  >LEN PROC-ARGV+
   s" tools/lint/lib.f"  >LEN PROC-ARGV+
   s" tools/json.f"  >LEN PROC-ARGV+
   s" bench/llm/validate-results-lib.f"  >LEN PROC-ARGV+
   s" bench/llm/expanded-report.f"  >LEN PROC-ARGV+
   s" --"  >LEN PROC-ARGV+
   ERT-RESULT$  >LEN PROC-ARGV+
   ERT-PERF$  >LEN PROC-ARGV+
   s" bin/hb" >LEN ERT-OUT ERT-CAPTURE-CAP >LEN
   ERT-ERR ERT-CAPTURE-CAP >LEN ERT-TIMEOUT-MS >MS
   RUN-ARGV-CAPTURE {: outu erru rc :}
   outu LEN>N erru LEN>N rc RC>N ;

: ERT-CONTAINS ( ptr u8 n -- ) {: a:ptr u :}
   ERT-OUT ERT-OUT-U @ a u FIND-SUB 0 >= TTRUE ;

: ERT-COPY-OUT2 ( -- )
   ERT-OUT ERT-OUT2 ERT-OUT-U @ BYTE-COPY
   ERT-OUT-U @ ERT-OUT2-U ! ;

: ERT-SAME-OUTPUT ( -- )
   ERT-OUT ERT-OUT-U @ ERT-OUT2 ERT-OUT2-U @ STR= TTRUE ;

: ERT-TEST-LATENCY ( -- )
   ERT-RUN-REPORT {: outu erru rc :}
   rc 0 T=
   erru 0 T=
   outu 0 > TTRUE
   outu ERT-OUT-U !
   s" ## Category by Arm and Model" ERT-CONTAINS
   s" | arithmetic | fixture | habu-forth | 1 | 1 | 100.00 | 100.00 | 1.00 | 9 | 0.01 | 100.00 | 100.00 |" ERT-CONTAINS
   s" ## LLM Feedback Latency" ERT-CONTAINS
   s" bench/llm/perf.f -- --json" ERT-CONTAINS
   s" | metric_validator | 34 | 0.03 |" ERT-CONTAINS
   s" | microbench_smoke | 56 | 0.06 |" ERT-CONTAINS
   ERT-COPY-OUT2
   ERT-RUN-REPORT {: outu2 erru2 rc2 :}
   rc2 0 T=
   erru2 0 T=
   outu2 0 > TTRUE
   outu2 ERT-OUT-U !
   ERT-SAME-OUTPUT ;

: ERT-MAIN ( -- )
   T-RESET
   ERT-PREPARE
   ERT-TEST-LATENCY
   CLEANUP-RUN
   T-REPORT
   s" expanded-report-test: ok" type cr ;

ERT-MAIN
