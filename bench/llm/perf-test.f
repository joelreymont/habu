\ perf-test.f - focused tests for the native LLM perf harness.

4096 constant PT-CAP
4000 constant PT-TIMEOUT-MS
64 constant PT-USAGE-RC

create PT-OUT PT-CAP allot
create PT-ERR PT-CAP allot

variable PT-OUT-U
variable PT-ERR-U
variable PT-RC
variable PT-ROOT
variable PT-ARR
variable PT-ROW

: PT-RESET ( -- )
   PROC-ARGV-ENV-RESET
   s" HABU_LLM_PERF_STUB" >LEN s" 1" >LEN PROC-ENV+
   PROC-ENV-INHERIT-MISSING ;

: PT-PERF-ARGV ( -- )
   s" --load"  >LEN PROC-ARGV+
   s" lib/errors.f"  >LEN PROC-ARGV+
   s" lib/string.f"  >LEN PROC-ARGV+
   s" lib/fs.f"  >LEN PROC-ARGV+
   s" lib/fs-mutate.f"  >LEN PROC-ARGV+
   s" lib/process.f"  >LEN PROC-ARGV+
   s" lib/process-argv.f"  >LEN PROC-ARGV+
   s" lib/process-env.f"  >LEN PROC-ARGV+
   s" lib/time.f"  >LEN PROC-ARGV+
   s" lib/memory.f"  >LEN PROC-ARGV+
   s" lib/json-write.f"  >LEN PROC-ARGV+
   s" bench/llm/perf-lib.f"  >LEN PROC-ARGV+
   s" bench/llm/perf.f"  >LEN PROC-ARGV+
   s" --"  >LEN PROC-ARGV+ ;

: PT-RUN ( -- )
   s" bin/hb" >LEN PT-OUT PT-CAP >LEN PT-ERR PT-CAP >LEN
   PT-TIMEOUT-MS >MS RUN-ARGV-ENV-CAPTURE {: outu erru rc :}
   rc RC>N PT-RC !
   erru LEN>N PT-ERR-U !
   outu LEN>N PT-OUT-U ! ;

: PT-RUN-PERF-JSON ( -- )
   PT-RESET
   PT-PERF-ARGV
   s" --json"  >LEN PROC-ARGV+
   PT-RUN ;

: PT-OUT$ ( -- ptr u8 n )
   PT-OUT PT-OUT-U @ ;

: PT-ERR$ ( -- ptr u8 n )
   PT-ERR PT-ERR-U @ ;

: PT-CONTAINS-OUT ( ptr u8 n -- ) {: a:ptr u :}
   PT-OUT$ a u CONTAINS? TTRUE ;

: PT-CONTAINS-ERR ( ptr u8 n -- ) {: a:ptr u :}
   PT-ERR$ a u CONTAINS? TTRUE ;

: PT-EXPECT-OK ( -- )
   PT-RC @ 0 T=
   PT-ERR-U @ 0 T= ;

: PT-JSON-GET ( n ptr u8 n -- n ) {: node:n key:ptr keyu :}
   node key keyu JSON-GET ;

: PT-JSON-S= ( n ptr u8 n -- ) {: node:n want:ptr wantu :}
   node JSON-STRING$ want wantu T$= ;

: PT-JSON-N= ( n ptr u8 n -- ) {: node:n want:ptr wantu :}
   node JSON-NUMBER$ want wantu T$= ;

: PT-ASSERT-RESULT ( n ptr u8 n -- ) {: row:n name:ptr nameu :}
   row s" name" PT-JSON-GET name nameu PT-JSON-S=
   row s" wall_ms" PT-JSON-GET JSON-KIND J-NUM T= ;

: PT-PARSE-JSON ( -- )
   PT-OUT$ JSON-PARSE PT-ROOT !
   PT-ROOT @ JSON-KIND J-OBJ T=
   PT-ROOT @ s" schema_version" PT-JSON-GET s" 1" PT-JSON-N=
   PT-ROOT @ s" bench" PT-JSON-GET s" llm-perf" PT-JSON-S=
   PT-ROOT @ s" results" PT-JSON-GET PT-ARR !
   PT-ARR @ JSON-KIND J-ARR T= ;

: PT-QUICK-JSON ( -- )
   PT-RUN-PERF-JSON
   PT-EXPECT-OK
   PT-PARSE-JSON
   PT-ROOT @ s" full" PT-JSON-GET JSON-BOOL@ TFALSE
   PT-ARR @ JSON-COUNT 5 T=
   PT-ARR @ 0 JSON-ARR@ s" check_solutions" PT-ASSERT-RESULT
   PT-ARR @ 1 JSON-ARR@ s" functional_tests" PT-ASSERT-RESULT
   PT-ARR @ 2 JSON-ARR@ s" metric_validator" PT-ASSERT-RESULT
   PT-ARR @ 3 JSON-ARR@ s" prop_smoke_250" PT-ASSERT-RESULT
   PT-ARR @ 4 JSON-ARR@ s" microbench_smoke" PT-ASSERT-RESULT ;

: PT-FULL-JSON ( -- )
   PT-RESET
   PT-PERF-ARGV
   s" --json"  >LEN PROC-ARGV+
   s" --full"  >LEN PROC-ARGV+
   PT-RUN
   PT-EXPECT-OK
   PT-PARSE-JSON
   PT-ROOT @ s" full" PT-JSON-GET JSON-BOOL@ TTRUE
   PT-ARR @ JSON-COUNT 8 T=
   PT-ARR @ 5 JSON-ARR@ s" self_rebuild" PT-ASSERT-RESULT
   PT-ARR @ 6 JSON-ARR@ s" hb_build_aot" PT-ASSERT-RESULT
   PT-ARR @ 7 JSON-ARR@ s" aot_runtime" PT-ASSERT-RESULT ;

: PT-TEXT-OUTPUT ( -- )
   PT-RESET
   PT-PERF-ARGV
   PT-RUN
   PT-EXPECT-OK
   s" llm-perf: mode=quick" PT-CONTAINS-OUT
   s" llm-perf: check_solutions" PT-CONTAINS-OUT
   s" llm-perf: metric_validator" PT-CONTAINS-OUT
   s" llm-perf: microbench_smoke" PT-CONTAINS-OUT
   s" ms" PT-CONTAINS-OUT ;

: PT-USAGE-ERROR ( -- )
   PT-RESET
   PT-PERF-ARGV
   s" --bogus"  >LEN PROC-ARGV+
   PT-RUN
   PT-RC @ PT-USAGE-RC T=
   s" usage: bench/llm/perf.f [--json] [--full]" PT-CONTAINS-ERR ;

: PT-DOUBLE-DASH-ERROR ( -- )
   PT-RESET
   PT-PERF-ARGV
   s" --"  >LEN PROC-ARGV+
   s" extra"  >LEN PROC-ARGV+
   PT-RUN
   PT-RC @ PT-USAGE-RC T=
   s" usage: bench/llm/perf.f [--json] [--full]" PT-CONTAINS-ERR ;

: PERF-TEST-MAIN ( -- )
   T-RESET
   PT-QUICK-JSON
   PT-FULL-JSON
   PT-TEXT-OUTPUT
   PT-USAGE-ERROR
   PT-DOUBLE-DASH-ERROR
   T-REPORT
   s" perf-test: ok" type cr ;

PERF-TEST-MAIN
