\ expanded-report.f - native expanded live benchmark Markdown report.
\
\ Load after lib/errors.f, lib/string.f, lib/fs.f, lib/process.f,
\ lib/process-argv.f, lib/argv.f, and tools/json.f.

66 constant ER-NOINPUT-RC
74 constant ER-IO-RC
10 constant ER-LF
30000 constant ER-VALIDATOR-TIMEOUT-MS
4096 constant ER-READ-CAP
65536 constant ER-SUM-CAP
65536 constant ER-JSON-CAP
65536 constant ER-PERF-CAP
16384 constant ER-ERR-CAP
45 constant ER-MINUS
46 constant ER-DOT
48 constant ER-ZERO
57 constant ER-NINE

create ER-READ-BUF ER-READ-CAP allot
create ER-SUM-BUF ER-SUM-CAP allot
create ER-JSON-BUF ER-JSON-CAP allot
create ER-PERF-BUF ER-PERF-CAP allot
create ER-ERR-BUF ER-ERR-CAP allot

variable ER-SUM-U
variable ER-JSON-U
variable ER-PERF-U
variable ER-READ-U
variable ER-FD
variable ER-ROWS
variable ER-SUM-ROOT
variable ER-FAM-ARR
variable ER-FAM-ROW
variable ER-PERF-ROOT
variable ER-PERF-ARR
variable ER-PERF-ROW
variable ER-PARSE-N

: ER-RESULT$ ( -- ptr u8 n )
   ARGV-POS# 0= if s" bench/llm/results/run-expanded.jsonl" else 0 ARGV-POS$ then ;

: ER-PERF$ ( -- ptr u8 n )
   1 ARGV-POS$ ;

: ER-TRUE ( -- bool )
   0 0= ;

: ER-FALSE ( -- bool )
   ER-TRUE 0= ;

: ER-ROW-COUNT ( -- n )
   ER-RESULT$ FS-PATHZ open-rd ER-FD !
   ER-FD @ 0 < if E-FS-OPEN throw then
   0 ER-ROWS !
   begin
      ER-FD @ ER-READ-BUF ER-READ-CAP read ER-READ-U !
      ER-READ-U @ 0 < if ER-FD @ close E-FS-IO throw then
      ER-READ-U @ 0 >
   while
      ER-READ-BUF ER-READ-U @ ER-LF COUNT-CHAR ER-ROWS @ + ER-ROWS !
   repeat
   ER-FD @ close
   ER-ROWS @ ;

: ER-CHECK-OPTS ( -- )
   ARGV-JSON? if s" expanded-report: --json is not a report option" ARGV-FAIL then
   ARGV-OUT? if s" expanded-report: -o is not a report option" ARGV-FAIL then ;

: ER-U. ( n -- ) {: n :}
   n 0 < if s" expanded-report: negative number" ER-IO-RC die then
   n STR-BASE >= if n STR-BASE / recurse then
   n STR-BASE mod STR-ZERO + emit ;

: ER-DASH. ( -- )
   ER-MINUS emit ;

: ER-DIGIT? ( n -- bool )
   dup ER-ZERO >= swap ER-NINE <= and ;

: ER-DIGIT> ( n -- n )
   ER-ZERO - ;

: ER-PARSE-U? ( ptr u8 i64 -- n bool ) {: a:ptr u :}
   u 0 <= if 0 ER-FALSE exit then
   0 ER-PARSE-N !
   0 begin dup u < while
      a over + c@ dup ER-DIGIT? 0= if 2drop 0 ER-FALSE exit then
      ER-DIGIT> ER-PARSE-N @ 10 * + ER-PARSE-N !
      1+
   repeat drop
   ER-PARSE-N @ ER-TRUE ;

: ER-SCALED. ( n -- ) {: n :}
   n 0 < if ER-DASH. exit then
   n 100 / ER-U.
   ER-DOT emit
   n 100 mod dup 10 < if ER-ZERO emit then
   ER-U. ;

: ER-SEC-CENTI ( n -- n )
   dup 0 < if exit then
   100 * 500 + 1000 / ;

: ER-RATIO-BP {: num den :} ( n n -- n )
   den 0= if 0 exit then
   num 10000 * den 2 / + den / ;

: ER-MEAN-CENTI {: sum count :} ( n n -- n )
   count 0= if 0 exit then
   sum 100 * count 2 / + count / ;

: ER-CONFIG ( -- )
   s" bench/llm/expanded-report.f [result.jsonl] [perf.json]" ARGV-USAGE!
   ARGV-PARSE
   ER-CHECK-OPTS
   0 2 ARGV-EXPECT-POS ;

: ER-REQUIRE-RESULT ( -- )
   ER-RESULT$ FILE? 0= if s" expanded-report: missing result file" ER-NOINPUT-RC die then ;

: ER-REQUIRE-PERF ( -- )
   ARGV-POS# 1 <= if exit then
   ER-PERF$ FILE? 0= if s" expanded-report: missing perf file" ER-NOINPUT-RC die then ;

: ER-VALIDATOR-BASE ( -- )
   PROC-ARGV-RESET
   s" --load"  >LEN PROC-ARGV+
   s" lib/errors.f"  >LEN PROC-ARGV+
   s" lib/memory.f"  >LEN PROC-ARGV+
   s" tools/date.f"  >LEN PROC-ARGV+
   s" tools/lint/text.f"  >LEN PROC-ARGV+ s" tools/lint/token.f" >LEN PROC-ARGV+ s" tools/lint/lib.f" >LEN PROC-ARGV+
   s" tools/json.f"  >LEN PROC-ARGV+
   s" tools/argv.f"  >LEN PROC-ARGV+
   s" bench/llm/validate-results.f"  >LEN PROC-ARGV+
   s" --"  >LEN PROC-ARGV+ ;

: ER-VALIDATOR-FAILED ( n -- ) {: erru :}
   erru 0 > if 2 ER-ERR-BUF erru write drop then
   s" expanded-report: validator failed" ER-IO-RC die ;

: ER-CHECK-RUN ( len len rc -- n ) {: outu erru rc :}
   rc RC>N 0 <> if erru LEN>N ER-VALIDATOR-FAILED then
   erru LEN>N 0 > if 2 ER-ERR-BUF erru LEN>N write drop then
   outu LEN>N ;

: ER-RUN-VALIDATOR-TEXT ( -- )
   ER-VALIDATOR-BASE
   ER-RESULT$  >LEN PROC-ARGV+
   s" bin/hb" >LEN ER-SUM-BUF ER-SUM-CAP >LEN
   ER-ERR-BUF ER-ERR-CAP >LEN ER-VALIDATOR-TIMEOUT-MS >MS RUN-ARGV-CAPTURE
   ER-CHECK-RUN ER-SUM-U ! ;

: ER-RUN-VALIDATOR-JSON ( -- )
   ER-VALIDATOR-BASE
   s" --json"  >LEN PROC-ARGV+
   ER-RESULT$  >LEN PROC-ARGV+
   s" bin/hb" >LEN ER-JSON-BUF ER-JSON-CAP >LEN
   ER-ERR-BUF ER-ERR-CAP >LEN ER-VALIDATOR-TIMEOUT-MS >MS RUN-ARGV-CAPTURE
   ER-CHECK-RUN ER-JSON-U ! ;

: ER-FENCE-TEXT ( -- )
   s" ```text" type cr
   ER-SUM-BUF ER-SUM-U @ type cr
   s" ```" type cr cr ;

: ER-FENCE-JSON ( -- )
   s" ```json" type cr
   ER-JSON-BUF ER-JSON-U @ type cr
   s" ```" type cr ;

: ER-PERF-GET ( n ptr u8 n -- n ) {: node:n a:ptr u:n :}
   node 0 < if -1 exit then
   node JSON-KIND J-OBJ <> if -1 exit then
   node a u JSON-GET ;

: ER-PERF-NUM? ( n -- bool )
   dup 0 < if drop ER-FALSE exit then
   JSON-KIND J-NUM = ;

: ER-PERF-NUM ( n -- n bool )
   dup ER-PERF-NUM? if JSON-NUMBER$ ER-PARSE-U? else drop 0 ER-FALSE then ;

: ER-CELL-GET ( ptr u8 n -- n )
   ER-FAM-ROW @ -rot ER-PERF-GET ;

: ER-CELL-N ( ptr u8 n -- n bool )
   ER-CELL-GET ER-PERF-NUM ;

: ER-CELL-N@ ( ptr u8 n -- n )
   ER-CELL-N 0= if drop 0 then ;

: ER-CELL-S. ( ptr u8 n -- )
   ER-CELL-GET dup 0 < if drop ER-DASH. exit then
   dup JSON-KIND J-STR <> if drop ER-DASH. exit then
   JSON-STRING$ type ;

: ER-CELL-U. ( ptr u8 n -- )
   ER-CELL-N if ER-U. else drop ER-DASH. then ;

: ER-CELL-BP. ( ptr u8 n -- )
   ER-CELL-N if ER-SCALED. else drop ER-DASH. then ;

: ER-CELL-FIRST. ( -- )
   s" first_tests_passed" ER-CELL-N@
   s" rows" ER-CELL-N@
   ER-RATIO-BP ER-SCALED. ;

: ER-CELL-ROUNDS. ( -- )
   s" rounds" ER-CELL-N@
   s" rows" ER-CELL-N@
   ER-MEAN-CENTI ER-SCALED. ;

: ER-CELL-WALL-S. ( -- )
   s" wall_ms" ER-CELL-N@ ER-SEC-CENTI ER-SCALED. ;

: ER-CELL-DIAG. ( -- )
   s" diagnostic_complete" ER-CELL-N@
   s" rows" ER-CELL-N@
   ER-RATIO-BP ER-SCALED. ;

: ER-CELL-REPLAY. ( -- )
   s" replay_ok" ER-CELL-N@
   s" rows" ER-CELL-N@
   ER-RATIO-BP ER-SCALED. ;

: ER-PERF-NUM. ( n -- )
   ER-PERF-NUM if ER-U. else drop ER-DASH. then ;

: ER-PERF-SEC. ( n -- )
   ER-PERF-NUM if ER-SEC-CENTI ER-SCALED. else drop ER-DASH. then ;

: ER-PERF-NOTE. ( -- )
   s" No perf JSON artifact was supplied with this report run." type cr cr ;

: ER-PERF-TABLE. ( -- )
   s" | check | wall ms | wall s |" type cr
   s" |---|---:|---:|" type cr
   0 begin dup ER-PERF-ARR @ JSON-COUNT < while
      ER-PERF-ARR @ over JSON-ARR@ ER-PERF-ROW !
      s" | " type
      ER-PERF-ROW @ s" name" ER-PERF-GET JSON-STRING$ type
      s"  | " type
      ER-PERF-ROW @ s" wall_ms" ER-PERF-GET dup ER-PERF-NUM.
      s"  | " type
      ER-PERF-SEC.
      s"  |" type cr
      1+
   repeat drop cr ;

: ER-FAMILY-NOTE. ( -- )
   s" Validator JSON did not include category/model/arm cells." type cr cr ;

: ER-FAMILY-HEADER. ( -- )
   s" | category | model | arm | rows | tests | pass@k | first pass | mean rounds | tokens | wall s | diagnostics | replay |" type cr
   s" |---|---|---|---:|---:|---:|---:|---:|---:|---:|---:|---:|" type cr ;

: ER-FAMILY-ROW. ( -- )
   s" | " type s" category" ER-CELL-S.
   s"  | " type s" model" ER-CELL-S.
   s"  | " type s" arm" ER-CELL-S.
   s"  | " type s" rows" ER-CELL-U.
   s"  | " type s" tests_passed" ER-CELL-U.
   s"  | " type s" task_pass_bp" ER-CELL-BP.
   s"  | " type ER-CELL-FIRST.
   s"  | " type ER-CELL-ROUNDS.
   s"  | " type s" tokens_used" ER-CELL-U.
   s"  | " type ER-CELL-WALL-S.
   s"  | " type ER-CELL-DIAG.
   s"  | " type ER-CELL-REPLAY.
   s"  |" type cr ;

: ER-FAMILY-TABLE. ( -- )
   ER-FAMILY-HEADER.
   0 begin dup ER-FAM-ARR @ JSON-COUNT < while
      ER-FAM-ARR @ over JSON-ARR@ ER-FAM-ROW !
      ER-FAMILY-ROW.
      1+
   repeat drop cr ;

: ER-FAMILY. ( -- )
   s" ## Category by Arm and Model" type cr cr
   ER-JSON-BUF ER-JSON-U @ JSON-PARSE ER-SUM-ROOT !
   ER-SUM-ROOT @ s" family_cells" ER-PERF-GET ER-FAM-ARR !
   ER-FAM-ARR @ 0 < if ER-FAMILY-NOTE. exit then
   ER-FAM-ARR @ JSON-KIND J-ARR <> if ER-FAMILY-NOTE. exit then
   ER-FAMILY-TABLE. ;

: ER-PERF. ( -- )
   s" ## LLM Feedback Latency" type cr cr
   s" Source command: `bench/llm/perf.f -- --json`. These timings measure local checker/test/validator/property/microbench latency, not model inference latency." type cr cr
   ARGV-POS# 1 <= if ER-PERF-NOTE. exit then
   ER-PERF$ ER-PERF-BUF ER-PERF-CAP READ-ALL ER-PERF-U !
   ER-PERF-BUF ER-PERF-U @ JSON-PARSE ER-PERF-ROOT !
   ER-PERF-ROOT @ s" results" ER-PERF-GET ER-PERF-ARR !
   ER-PERF-ARR @ 0 < if ER-PERF-NOTE. exit then
   ER-PERF-TABLE. ;

: ER-REPORT. ( -- )
   s" # Expanded Habu Forth Live Benchmark" type cr cr
   s" Generated: `deterministic from raw evidence`" type cr cr
   s" Raw evidence: `" type ER-RESULT$ type s" ` (" type ER-ROW-COUNT ER-U. s"  rows)" type cr cr
   s" The raw JSONL rows are validated by `bench/llm/validate-results.f`; replay artifacts are embedded in every row with SHA-256 fields." type cr cr
   s" ## Validator Summary" type cr cr
   ER-FENCE-TEXT
   ER-FAMILY.
   s" ## JSON Summary" type cr cr
   ER-FENCE-JSON
   cr cr
   ER-PERF. ;

ER-CONFIG
ER-REQUIRE-RESULT
ER-REQUIRE-PERF
ER-RUN-VALIDATOR-TEXT
ER-RUN-VALIDATOR-JSON
ER-REPORT.
