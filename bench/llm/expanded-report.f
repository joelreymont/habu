\ expanded-report.f - native expanded live benchmark Markdown report.
\
\ Load after lib/errors.f, lib/string.f, lib/memory.f, lib/fs.f, lib/argv.f, tools/json.f,
\ and bench/llm/validate-results-lib.f.

66 constant ER-NOINPUT-RC
74 constant ER-IO-RC
10 constant ER-LF
4096 constant ER-READ-CAP
65536 constant ER-PERF-CAP
45 constant ER-MINUS
46 constant ER-DOT
48 constant ER-ZERO
57 constant ER-NINE

create ER-READ-BUF ER-READ-CAP allot
create ER-PERF-BUF ER-PERF-CAP allot

variable ER-PERF-U
variable ER-READ-U
variable ER-FD
variable ER-ROWS
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

: ER-VALIDATE ( -- )
   LV-MODE-SUMMARY LV-MODE !
   ER-RESULT$ LV-RESULT-PATH!
   LV-SCAN-TASKS
   LV-SCAN-RESULTS ;

: ER-FENCE-TEXT ( -- )
   s" ```text" type cr
   LV-OUTPUT-SUMMARY-TEXT
   s" ```" type cr cr ;

: ER-FENCE-JSON ( -- )
   s" ```json" type cr
   LV-OUTPUT-SUMMARY-JSON
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

: ER-FAMILY-ROW. ( n -- ) {: k :}
   s" | " type k LV-FAM-CAT$ type
   s"  | " type k LV-FAM-MODEL$ type
   s"  | " type k LV-FAM-ARM$ type
   s"  | " type k LV-FAM-ROWS@ ER-U.
   s"  | " type k LV-FAM-TESTS@ ER-U.
   s"  | " type k LV-FAM-PASS-GROUPS k LV-FAM-GROUPS ER-RATIO-BP ER-SCALED.
   s"  | " type k LV-FAM-FIRST@ k LV-FAM-ROWS@ ER-RATIO-BP ER-SCALED.
   s"  | " type k LV-FAM-ROUNDS@ k LV-FAM-ROWS@ ER-MEAN-CENTI ER-SCALED.
   s"  | " type k LV-FAM-TOKENS@ ER-U.
   s"  | " type k LV-FAM-WALL@ ER-SEC-CENTI ER-SCALED.
   s"  | " type k LV-FAM-DIAG-OK@ k LV-FAM-ROWS@ ER-RATIO-BP ER-SCALED.
   s"  | " type k LV-FAM-REPLAY@ k LV-FAM-ROWS@ ER-RATIO-BP ER-SCALED.
   s"  |" type cr ;

: ER-FAMILY-TABLE. ( -- )
   ER-FAMILY-HEADER.
   0 begin dup LV-FAM# @ < while
      dup ER-FAMILY-ROW.
      1+
   repeat drop cr ;

: ER-FAMILY. ( -- )
   s" ## Category by Arm and Model" type cr cr
   LV-FAM# @ 0= if ER-FAMILY-NOTE. exit then
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
ER-VALIDATE
ER-REPORT.
