\ expanded-report.f - native expanded live benchmark Markdown report.
\
\ Load after lib/errors.f, lib/string.f, lib/fs.f, lib/process.f,
\ lib/process-argv.f, lib/time.f, lib/date.f, and lib/argv.f.

66 constant ER-NOINPUT-RC
74 constant ER-IO-RC
10 constant ER-LF
30000 constant ER-VALIDATOR-TIMEOUT-MS
4096 constant ER-READ-CAP
65536 constant ER-SUM-CAP
65536 constant ER-JSON-CAP
16384 constant ER-ERR-CAP

create ER-DATE-BUF DATE-TIME-LEN allot
create ER-READ-BUF ER-READ-CAP allot
create ER-SUM-BUF ER-SUM-CAP allot
create ER-JSON-BUF ER-JSON-CAP allot
create ER-ERR-BUF ER-ERR-CAP allot

variable ER-SUM-U
variable ER-JSON-U
variable ER-READ-U
variable ER-FD
variable ER-ROWS

: ER-RESULT$ ( -- ptr u8 n )
   ARGV-POS# 0= if s" bench/llm/results/run-expanded.jsonl" else 0 ARGV-POS$ then ;

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

: ER-CONFIG ( -- )
   s" bench/llm/expanded-report.f [result.jsonl]" ARGV-USAGE!
   ARGV-PARSE
   ER-CHECK-OPTS
   0 1 ARGV-EXPECT-POS ;

: ER-REQUIRE-RESULT ( -- )
   ER-RESULT$ FILE? 0= if s" expanded-report: missing result file" ER-NOINPUT-RC die then ;

: ER-VALIDATOR-BASE ( -- )
   PROC-ARGV-RESET
   s" --load" PROC-ARGV+
   s" tools/date.f" PROC-ARGV+
   s" tools/lint/lib.f" PROC-ARGV+
   s" tools/json.f" PROC-ARGV+
   s" tools/argv.f" PROC-ARGV+
   s" bench/llm/validate-results.f" PROC-ARGV+
   s" --" PROC-ARGV+ ;

: ER-VALIDATOR-FAILED ( n -- ) {: erru :}
   erru 0 > if 2 ER-ERR-BUF erru write drop then
   s" expanded-report: validator failed" ER-IO-RC die ;

: ER-CHECK-RUN ( n n n -- n ) {: outu erru rc :}
   rc 0 <> if erru ER-VALIDATOR-FAILED then
   erru 0 > if 2 ER-ERR-BUF erru write drop then
   outu ;

: ER-RUN-VALIDATOR-TEXT ( -- )
   ER-VALIDATOR-BASE
   ER-RESULT$ PROC-ARGV+
   s" bin/hb" ER-SUM-BUF ER-SUM-CAP ER-ERR-BUF ER-ERR-CAP ER-VALIDATOR-TIMEOUT-MS RUN-ARGV-CAPTURE
   ER-CHECK-RUN ER-SUM-U ! ;

: ER-RUN-VALIDATOR-JSON ( -- )
   ER-VALIDATOR-BASE
   s" --json" PROC-ARGV+
   ER-RESULT$ PROC-ARGV+
   s" bin/hb" ER-JSON-BUF ER-JSON-CAP ER-ERR-BUF ER-ERR-CAP ER-VALIDATOR-TIMEOUT-MS RUN-ARGV-CAPTURE
   ER-CHECK-RUN ER-JSON-U ! ;

: ER-FENCE-TEXT ( -- )
   s" ```text" type cr
   ER-SUM-BUF ER-SUM-U @ type cr
   s" ```" type cr cr ;

: ER-FENCE-JSON ( -- )
   s" ```json" type cr
   ER-JSON-BUF ER-JSON-U @ type cr
   s" ```" type cr ;

: ER-REPORT. ( -- )
   TIME-EPOCH-SECONDS ER-DATE-BUF DATE-TIME-LEN FORMAT-EPOCH-UTC 2drop
   s" # Expanded Habu Forth Live Benchmark" type cr cr
   s" Generated: `" type ER-DATE-BUF DATE-TIME-LEN type s" `" type cr cr
   s" Raw evidence: `" type ER-RESULT$ type s" ` (" type ER-ROW-COUNT ER-U. s"  rows)" type cr cr
   s" The raw JSONL rows are validated by `bench/llm/validate-results.f`; replay artifacts are embedded in every row with SHA-256 fields." type cr cr
   s" ## Validator Summary" type cr cr
   ER-FENCE-TEXT
   s" ## JSON Summary" type cr cr
   ER-FENCE-JSON ;

ER-CONFIG
ER-REQUIRE-RESULT
ER-RUN-VALIDATOR-TEXT
ER-RUN-VALIDATOR-JSON
ER-REPORT.
