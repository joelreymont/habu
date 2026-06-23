\ report-test.f - focused checked fixtures for bench/llm/report.f.
\
\ Load after lib/errors.f, lib/string.f, lib/test.f, lib/memory.f, lib/json-write.f,
\ lib/fs.f, lib/fs-mutate.f, lib/process.f, and lib/process-argv.f.

65536 constant RPT-CAPTURE-CAP
2000 constant RPT-TIMEOUT-MS
10 constant RPT-LF

create RPT-ROOT FS-PATH-CAP allot
create RPT-RESULT FS-PATH-CAP allot
create RPT-OUT RPT-CAPTURE-CAP allot
create RPT-ERR RPT-CAPTURE-CAP allot
create RPT-LF-BUF 1 allot

variable RPT-ROOT-U
variable RPT-RESULT-U
variable RPT-OUT-U

RPT-LF RPT-LF-BUF c!

: RPT-COPY! ( ptr u8 n ptr u8 ptr n -- ) {: a:ptr u dst:ptr lenp:ptr :}
   u FS-PATH-CAP > if E-FS-CAPACITY throw then
   a dst u BYTE-COPY
   u lenp ! ;

: RPT-ROOT$ ( -- ptr u8 n )
   RPT-ROOT RPT-ROOT-U @ ;

: RPT-RESULT$ ( -- ptr u8 n )
   RPT-RESULT RPT-RESULT-U @ ;

: RPT-TRUE ( -- bool )
   0 0= ;

: RPT-FALSE ( -- bool )
   RPT-TRUE 0= ;

: RPT-FIELD-S ( ptr u8 n ptr u8 n -- )
   JW-COMMA
   JW-FIELD-S ;

: RPT-FIELD-U ( ptr u8 n n -- )
   JW-COMMA
   JW-FIELD-U ;

: RPT-FIELD-BOOL ( ptr u8 n bool -- )
   JW-COMMA
   JW-FIELD-BOOL ;

: RPT-FIELD-NULL ( ptr u8 n -- )
   JW-COMMA
   JW-FIELD-NULL ;

: RPT-FIELD-RUNTIME ( n -- ) {: runtime :}
   runtime 0 < if s" runtime_ms" RPT-FIELD-NULL exit then
   s" runtime_ms" runtime RPT-FIELD-U ;

: RPT-DIAGS ( bool -- ) {: ok :}
   s" diagnostic_token" ok RPT-FIELD-BOOL
   s" diagnostic_span" ok RPT-FIELD-BOOL
   s" diagnostic_expected" ok RPT-FIELD-BOOL
   s" diagnostic_actual" ok RPT-FIELD-BOOL
   s" diagnostic_code" ok RPT-FIELD-BOOL
   s" diagnostic_repair_class" ok RPT-FIELD-BOOL
   s" all_errors_stable" ok RPT-FIELD-BOOL ;

: RPT-APPEND-ROW ( -- )
   RPT-RESULT$ JW$ APPEND-FILE
   RPT-RESULT$ RPT-LF-BUF 1 APPEND-FILE ;

: RPT-CLEAR ( -- )
   RPT-RESULT$ s" " WRITE-ALL ;

: RPT-ROW ( n ptr u8 n ptr u8 n ptr u8 n ptr u8 n ptr u8 n n bool bool n n n bool -- )
   {: task name:ptr nameu model:ptr modelu arm:ptr armu family:ptr familyu outcome:ptr outcomeu rounds first false-reject tokens wall runtime diag :}
   JW-RESET
   JW-OBJECT-START
   s" task_id" task JW-FIELD-U
   s" name" name nameu RPT-FIELD-S
   s" model_id" model modelu RPT-FIELD-S
   s" model" model modelu RPT-FIELD-S
   s" arm" arm armu RPT-FIELD-S
   s" task_family" family familyu RPT-FIELD-S
   s" outcome" outcome outcomeu RPT-FIELD-S
   s" rounds" rounds RPT-FIELD-U
   s" first_pass" first RPT-FIELD-BOOL
   s" checker_false_reject" false-reject RPT-FIELD-BOOL
   s" tokens" tokens RPT-FIELD-U
   s" wall_ms" wall RPT-FIELD-U
   runtime RPT-FIELD-RUNTIME
   s" runtime_repetitions" 2 RPT-FIELD-U
   s" runtime_warmups" 1 RPT-FIELD-U
   s" runtime_status" s" ok" RPT-FIELD-S
   diag RPT-DIAGS
   JW-OBJECT-END
   RPT-APPEND-ROW ;

: RPT-PASS-ROW ( n ptr u8 n ptr u8 n ptr u8 n ptr u8 n n n n -- )
   {: task name:ptr nameu model:ptr modelu arm:ptr armu family:ptr familyu tokens wall runtime :}
   task name nameu model modelu arm armu family familyu s" pass" 1 RPT-TRUE RPT-FALSE tokens wall runtime RPT-TRUE RPT-ROW ;

: RPT-FAIL-ROW ( n ptr u8 n ptr u8 n ptr u8 n ptr u8 n n n n -- )
   {: task name:ptr nameu model:ptr modelu arm:ptr armu family:ptr familyu tokens wall runtime :}
   task name nameu model modelu arm armu family familyu s" fail" 2 RPT-FALSE RPT-FALSE tokens wall runtime RPT-FALSE RPT-ROW ;

: RPT-FALSE-REJECT-ROW ( n ptr u8 n ptr u8 n ptr u8 n ptr u8 n n n n -- )
   {: task name:ptr nameu model:ptr modelu arm:ptr armu family:ptr familyu tokens wall runtime :}
   task name nameu model modelu arm armu family familyu s" pass" 2 RPT-FALSE RPT-TRUE tokens wall runtime RPT-TRUE RPT-ROW ;

: RPT-PREPARE ( -- )
   CLEANUP-RESET
   s" habu-report-test" TMPDIR-MKDIR RPT-ROOT RPT-ROOT-U RPT-COPY!
   RPT-ROOT$ CLEANUP-TREE+
   RPT-ROOT$ s" run.jsonl" RPT-RESULT JOIN-PATH RPT-RESULT-U !
   RPT-CLEAR ;

: RPT-RUN-REPORT ( -- n n n )
   PROC-ARGV-RESET
   s" --load"  >LEN PROC-ARGV+
   s" lib/errors.f"  >LEN PROC-ARGV+
   s" lib/string.f"  >LEN PROC-ARGV+
   s" lib/memory.f"  >LEN PROC-ARGV+
   s" lib/fs.f"  >LEN PROC-ARGV+
   s" tools/json.f"  >LEN PROC-ARGV+
   s" tools/json-file.f"  >LEN PROC-ARGV+
   s" tools/argv.f"  >LEN PROC-ARGV+
   s" bench/llm/report.f"  >LEN PROC-ARGV+
   s" --"  >LEN PROC-ARGV+
   RPT-RESULT$  >LEN PROC-ARGV+
   s" bin/hb" >LEN RPT-OUT RPT-CAPTURE-CAP >LEN
   RPT-ERR RPT-CAPTURE-CAP >LEN RPT-TIMEOUT-MS >MS
   RUN-ARGV-CAPTURE {: outu erru rc :}
   outu LEN>N erru LEN>N rc RC>N ;

: RPT-CAPTURE ( -- )
   RPT-RUN-REPORT {: outu erru rc :}
   rc 0 T=
   erru 0 T=
   outu 0 > TTRUE
   outu RPT-OUT-U ! ;

: RPT-CONTAINS ( ptr u8 n -- ) {: a:ptr u :}
   RPT-OUT RPT-OUT-U @ a u FIND-SUB 0 >= TTRUE ;

: RPT-TEST-ZERO-TOKENS ( -- )
   RPT-CLEAR
   1 s" ZERO-TOK" s" fixture" s" habu-a" s" arrays" 0 10 -1 RPT-PASS-ROW
   1 s" ZERO-TOK" s" fixture" s" js" s" arrays" 5 10 -1 RPT-PASS-ROW
   RPT-CAPTURE
   s" exclude 1 passing row" RPT-CONTAINS
   s" ZERO-TOK" RPT-CONTAINS
   s" Habu raw 1" RPT-CONTAINS ;

: RPT-TEST-MODELS ( -- )
   RPT-CLEAR
   1 s" MREG" s" alpha" s" js" s" arrays" 5 10 -1 RPT-PASS-ROW
   1 s" MREG" s" beta" s" js" s" arrays" 9 20 -1 RPT-FAIL-ROW
   RPT-CAPTURE
   s" ## Per-Model Reliability" RPT-CONTAINS
   s" | alpha | JavaScript | 1 | 1 | 100%" RPT-CONTAINS
   s" | beta | JavaScript | 1 | 0 | 0%" RPT-CONTAINS ;

: RPT-TEST-RUNTIME ( -- )
   RPT-CLEAR
   1 s" RT" s" fixture" s" js" s" arrays" 5 9000 7 RPT-PASS-ROW
   RPT-CAPTURE
   s" median runtime ms" RPT-CONTAINS
   s" | JavaScript | 1 | 5 | **5** | 5 | 7 | 7 | 9 | 9 |" RPT-CONTAINS
   s" ## Limitations" RPT-CONTAINS
   s" deterministic-vs-live boundary" RPT-CONTAINS ;

: RPT-TEST-ARM-LABELS ( -- )
   RPT-CLEAR
   1 s" HARM" s" fixture" s" habu-stdlib" s" arrays" 5 10 1 RPT-PASS-ROW
   1 s" HARM" s" fixture" s" habu-skeleton" s" arrays" 6 10 1 RPT-PASS-ROW
   2 s" PYTS" s" fixture" s" ts" s" arrays" 6 10 1 RPT-PASS-ROW
   RPT-CAPTURE
   s" Habu + stdlib" RPT-CONTAINS
   s" Habu + skeleton" RPT-CONTAINS
   s" TypeScript" RPT-CONTAINS ;

: RPT-TEST-CATEGORY-DELTAS ( -- )
   RPT-CLEAR
   1 s" ARR-A" s" fixture" s" habu-a" s" arrays" 100 10 10 RPT-PASS-ROW
   2 s" ARR-B" s" fixture" s" habu-a" s" arrays" 0 20 -1 RPT-FAIL-ROW
   1 s" ARR-A" s" fixture" s" habu-stdlib" s" arrays" 50 10 8 RPT-PASS-ROW
   2 s" ARR-B" s" fixture" s" habu-stdlib" s" arrays" 60 10 12 RPT-PASS-ROW
   1 s" ARR-A" s" fixture" s" habu-skeleton" s" arrays" 80 10 20 RPT-PASS-ROW
   2 s" ARR-B" s" fixture" s" habu-skeleton" s" arrays" 0 20 -1 RPT-FAIL-ROW
   3 s" STR-A" s" fixture" s" habu-a" s" strings" 200 10 30 RPT-PASS-ROW
   3 s" STR-A" s" fixture" s" habu-stdlib" s" strings" 100 10 12 RPT-PASS-ROW
   3 s" STR-A" s" fixture" s" habu-skeleton" s" strings" 90 10 14 RPT-PASS-ROW
   RPT-CAPTURE
   s" ## Category Reliability And Effort" RPT-CONTAINS
   s" ## Habu Arm Deltas By Category" RPT-CONTAINS
   s" | arrays | 50%" RPT-CONTAINS
   s" +50pp" RPT-CONTAINS ;

: RPT-TEST-FALSE-REJECT ( -- )
   RPT-CLEAR
   1 s" FJR" s" fixture" s" habu-a" s" arrays" 5 10 1 RPT-FALSE-REJECT-ROW
   1 s" FJR" s" fixture" s" js" s" arrays" 5 10 1 RPT-PASS-ROW
   RPT-CAPTURE
   s" Checker false-reject rows: 1" RPT-CONTAINS
   s" execution-confirmed" RPT-CONTAINS
   s" Habu raw 1" RPT-CONTAINS ;

: RPT-TEST-TASK-TABLE-ROWS ( -- )
   RPT-CLEAR
   1 s" TASK-A" s" fixture" s" habu-a" s" arrays" 100 10 1 RPT-PASS-ROW
   1 s" TASK-A" s" fixture" s" js" s" arrays" 50 10 1 RPT-PASS-ROW
   2 s" TASK-B" s" fixture" s" habu-a" s" arrays" 75 10 1 RPT-PASS-ROW
   2 s" TASK-B" s" fixture" s" js" s" arrays" 25 10 1 RPT-PASS-ROW
   RPT-CAPTURE
   s" | TASK-A | 100 |" RPT-CONTAINS
   s" | TASK-B | 75 |" RPT-CONTAINS
   s" raw pass/1" RPT-CONTAINS ;

: RPT-MAIN ( -- )
   T-RESET
   RPT-PREPARE
   RPT-TEST-ZERO-TOKENS
   RPT-TEST-MODELS
   RPT-TEST-RUNTIME
   RPT-TEST-ARM-LABELS
   RPT-TEST-CATEGORY-DELTAS
   RPT-TEST-FALSE-REJECT
   RPT-TEST-TASK-TABLE-ROWS
   CLEANUP-RUN
   T-REPORT
   s" report-test: ok" type cr ;

RPT-MAIN
