\ perf-lib.f - native LLM feedback-loop latency benchmark.
\
\ Load after lib/errors.f, lib/string.f, lib/fs.f, lib/fs-mutate.f,
\ lib/process.f, lib/process-argv.f, lib/process-env.f, lib/time.f, and
\ lib/memory.f and lib/json-write.f.

64 constant PERF-USAGE-RC
69 constant PERF-NOHB-RC
74 constant PERF-RUN-RC
1000000 constant PERF-NS-PER-MS
120000 constant PERF-TIMEOUT-MS
$40000 constant PERF-SRC-CAP
$100000 constant PERF-CAPTURE-CAP
10 constant PERF-LF
34 constant PERF-DQ
44 constant PERF-COMMA
58 constant PERF-COLON
91 constant PERF-LBRACK
123 constant PERF-LBRACE

create PERF-SRC PERF-SRC-CAP allot
create PERF-OUT PERF-CAPTURE-CAP allot
create PERF-ERR PERF-CAPTURE-CAP allot
create PERF-ROOT FS-PATH-CAP allot
create PERF-AOT-SRC FS-PATH-CAP allot
create PERF-AOT-BIN FS-PATH-CAP allot

variable PERF-JSON
variable PERF-FULL
variable PERF-FIRST
variable PERF-ARG-I
variable PERF-SRC-U
variable PERF-RD
variable PERF-START
variable PERF-OUT-U
variable PERF-ERR-U
variable PERF-RC
variable PERF-MS
variable PERF-ROOT-U
variable PERF-AOT-SRC-U
variable PERF-AOT-BIN-U

: PERF-TRUE ( -- bool )
   0 0= ;

: PERF-FALSE ( -- bool )
   PERF-TRUE 0= ;

: PERF-COPY! ( ptr u8 n ptr u8 ptr n -- ) {: a:ptr u dst:ptr up:ptr :}
   u 0 < if E-FS-PATH throw then
   u FS-PATH-CAP > if E-FS-PATH throw then
   a dst u BYTE-COPY
   u up ! ;

: PERF-ROOT$ ( -- ptr u8 n )
   PERF-ROOT PERF-ROOT-U @ ;

: PERF-AOT-SRC$ ( -- ptr u8 n )
   PERF-AOT-SRC PERF-AOT-SRC-U @ ;

: PERF-AOT-BIN$ ( -- ptr u8 n )
   PERF-AOT-BIN PERF-AOT-BIN-U @ ;

: PERF-USAGE ( -- )
   s" usage: bench/llm/perf.f [--json] [--full]" PERF-USAGE-RC die ;

: PERF-PARSE-TOK ( ptr u8 n -- ) {: a:ptr u :}
   a u s" --json" STR= if -1 PERF-JSON ! exit then
   a u s" --full" STR= if -1 PERF-FULL ! exit then
   a u s" --" STR= if
      PERF-ARG-I @ 1+ SCRIPT-ARGC <> if PERF-USAGE then
      SCRIPT-ARGC PERF-ARG-I !
      exit
   then
   PERF-USAGE ;

: PERF-PARSE ( -- )
   0 PERF-JSON !
   0 PERF-FULL !
   0 PERF-ARG-I !
   begin PERF-ARG-I @ SCRIPT-ARGC < while
      PERF-ARG-I @ SCRIPT-ARGV$ PERF-PARSE-TOK
      PERF-ARG-I @ 1+ PERF-ARG-I !
   repeat ;

: PERF-REQUIRE-HB ( -- )
   s" bin/hb" EXECUTABLE? 0= if
      s" llm-perf: no bin/hb - recover with docs/seed.md" PERF-NOHB-RC die
   then ;

: PERF-STUB? ( -- bool )
   s" HABU_LLM_PERF_STUB" GETENV nip 0 > ;

: PERF-SRC-RESET ( -- )
   0 PERF-SRC-U ! ;

: PERF-SRC+ ( ptr u8 n -- ) {: path:ptr pathu :}
   path pathu PERF-SRC PERF-SRC-U @ + PERF-SRC-CAP PERF-SRC-U @ -
   READ-ALL PERF-RD !
   PERF-SRC-U @ PERF-RD @ + PERF-SRC-U ! ;

: PERF-SRC$ ( -- ptr u8 n )
   PERF-SRC PERF-SRC-U @ ;

: PERF-ELAPSED-MS ( n n -- n ) {: start:n stop:n :}
   stop start - PERF-NS-PER-MS 1- + PERF-NS-PER-MS / ;

: PERF-CAPTURE! ( len len rc -- ) {: outu erru rc :}
   rc RC>N PERF-RC !
   erru LEN>N PERF-ERR-U !
   outu LEN>N PERF-OUT-U ! ;

: PERF-FAIL ( ptr u8 n -- ) {: name:ptr nameu :}
   PERF-JSON @ 0 <> if s" ]}" type cr then
   2 s" llm-perf: " write drop
   2 name nameu write drop
   2 s"  failed" write drop
   PERF-ERR-U @ 0 > if 2 PERF-ERR PERF-ERR-U @ write drop then
   s" llm-perf: child command failed" PERF-RC @ die ;

: PERF-JSON-RESULT ( ptr u8 n n -- ) {: name:ptr nameu ms:n :}
   PERF-FIRST @ 0= if PERF-COMMA emit then
   JW-RESET
   JW-OBJECT-START
   s" name" name nameu JW-FIELD-S
   JW-COMMA
   s" wall_ms" ms JW-FIELD-U
   JW-OBJECT-END
   JW$ type
   0 PERF-FIRST ! ;

: PERF-TEXT-RESULT ( ptr u8 n n -- ) {: name:ptr nameu ms:n :}
   s" llm-perf: " type
   name nameu type
   s"  " type
   ms .
   s" ms" type cr ;

: PERF-RECORD ( ptr u8 n n -- ) {: name:ptr nameu ms:n :}
   PERF-JSON @ 0 <> if
      name nameu ms PERF-JSON-RESULT
   else
      name nameu ms PERF-TEXT-RESULT
   then ;

: PERF-MEASURE-ARGV ( ptr u8 n -- ) {: name:ptr nameu :}
   TIME-MONO-NS PERF-START !
   s" bin/hb" >LEN PERF-OUT PERF-CAPTURE-CAP >LEN
   PERF-ERR PERF-CAPTURE-CAP >LEN PERF-TIMEOUT-MS >MS
   RUN-ARGV-CAPTURE PERF-CAPTURE!
   PERF-START @ TIME-MONO-NS PERF-ELAPSED-MS PERF-MS !
   PERF-RC @ 0 <> if name nameu PERF-FAIL then
   name nameu PERF-MS @ PERF-RECORD ;

: PERF-MEASURE-STDIN ( ptr u8 n ptr u8 n -- ) {: name:ptr nameu src:ptr srcu :}
   TIME-MONO-NS PERF-START !
   s" bin/hb" >LEN src srcu >LEN PERF-OUT PERF-CAPTURE-CAP >LEN
   PERF-ERR PERF-CAPTURE-CAP >LEN PERF-TIMEOUT-MS >MS
   RUN-ARGV-STDIN-CAPTURE PERF-CAPTURE!
   PERF-START @ TIME-MONO-NS PERF-ELAPSED-MS PERF-MS !
   PERF-RC @ 0 <> if name nameu PERF-FAIL then
   name nameu PERF-MS @ PERF-RECORD ;

: PERF-MEASURE-PATH ( ptr u8 n ptr u8 n -- ) {: name:ptr nameu path:ptr pathu :}
   TIME-MONO-NS PERF-START !
   path pathu >LEN PERF-OUT PERF-CAPTURE-CAP >LEN
   PERF-ERR PERF-CAPTURE-CAP >LEN PERF-TIMEOUT-MS >MS
   RUN-CAPTURE PERF-CAPTURE!
   PERF-START @ TIME-MONO-NS PERF-ELAPSED-MS PERF-MS !
   PERF-RC @ 0 <> if name nameu PERF-FAIL then
   name nameu PERF-MS @ PERF-RECORD ;

: PERF-STUB-SRC$ ( -- ptr u8 n )
   s" 1 1 + . cr" ;

: PERF-MEASURE-STUB ( ptr u8 n -- ) {: name:ptr nameu :}
   PROC-ARGV-RESET
   name nameu PERF-STUB-SRC$ PERF-MEASURE-STDIN ;

: PERF-CHECK-ARGS ( -- )
   PROC-ARGV-RESET
   s" --load"  >LEN PROC-ARGV+
   s" lib/errors.f"  >LEN PROC-ARGV+
   s" lib/string.f"  >LEN PROC-ARGV+
   s" lib/fs.f"  >LEN PROC-ARGV+
   s" lib/fs-mutate.f"  >LEN PROC-ARGV+
   s" lib/process.f"  >LEN PROC-ARGV+
   s" lib/process-argv.f"  >LEN PROC-ARGV+
   s" lib/source.f"  >LEN PROC-ARGV+
   s" tools/argv.f"  >LEN PROC-ARGV+
   s" tools/check.f"  >LEN PROC-ARGV+
   s" --"  >LEN PROC-ARGV+
   s" bench/llm/solutions.f"  >LEN PROC-ARGV+ ;

: PERF-CHECK-SOLUTIONS ( -- )
   PERF-STUB? if s" check_solutions" PERF-MEASURE-STUB exit then
   PERF-CHECK-ARGS
   s" check_solutions" PERF-MEASURE-ARGV ;

: PERF-FUNCTIONAL-TESTS ( -- )
   PERF-STUB? if s" functional_tests" PERF-MEASURE-STUB exit then
   PROC-ARGV-RESET
   PERF-SRC-RESET
   s" bench/llm/solutions.f" PERF-SRC+
   s" bench/llm/tests.f" PERF-SRC+
   s" functional_tests" PERF-SRC$ PERF-MEASURE-STDIN ;

: PERF-VALIDATOR-ARGS ( -- )
   PROC-ARGV-RESET
   s" --load"  >LEN PROC-ARGV+
   s" lib/errors.f"  >LEN PROC-ARGV+
   s" lib/string.f"  >LEN PROC-ARGV+
   s" lib/memory.f"  >LEN PROC-ARGV+
   s" lib/fs.f"  >LEN PROC-ARGV+
   s" tools/date.f"  >LEN PROC-ARGV+
   s" tools/lint/text.f"  >LEN PROC-ARGV+ s" tools/lint/token.f" >LEN PROC-ARGV+ s" tools/lint/lib.f" >LEN PROC-ARGV+
   s" tools/json.f"  >LEN PROC-ARGV+
   s" tools/json-file.f"  >LEN PROC-ARGV+
   s" tools/argv.f"  >LEN PROC-ARGV+
   s" bench/llm/validate-results-lib.f"  >LEN PROC-ARGV+
   s" bench/llm/validate-results.f"  >LEN PROC-ARGV+
   s" --"  >LEN PROC-ARGV+ ;

: PERF-METRIC-VALIDATOR ( -- )
   PERF-STUB? if s" metric_validator" PERF-MEASURE-STUB exit then
   PERF-VALIDATOR-ARGS
   s" metric_validator" PERF-MEASURE-ARGV ;

: PERF-PROP-SMOKE ( -- )
   PERF-STUB? if s" prop_smoke_250" PERF-MEASURE-STUB exit then
   PROC-ARGV-RESET
   s" 123"  >LEN PROC-ARGV+
   s" 250"  >LEN PROC-ARGV+
   PERF-SRC-RESET
   s" test/prop-test.f" PERF-SRC+
   s" prop_smoke_250" PERF-SRC$ PERF-MEASURE-STDIN ;

: PERF-MICROBENCH-SMOKE ( -- )
   PERF-STUB? if s" microbench_smoke" PERF-MEASURE-STUB exit then
   PROC-ARGV-RESET
   s" tools/bench.f"  >LEN PROC-ARGV+
   s" --smoke"  >LEN PROC-ARGV+
   s" microbench_smoke" PERF-MEASURE-ARGV ;

: PERF-INSTALL-ARGS ( -- )
   PROC-ARGV-RESET
   s" --load"  >LEN PROC-ARGV+
   s" lib/errors.f"  >LEN PROC-ARGV+
   s" lib/string.f"  >LEN PROC-ARGV+
   s" lib/fs.f"  >LEN PROC-ARGV+
   s" lib/fs-mutate.f"  >LEN PROC-ARGV+
   s" lib/process.f"  >LEN PROC-ARGV+
   s" lib/process-argv.f"  >LEN PROC-ARGV+
   s" lib/process-env.f"  >LEN PROC-ARGV+
   s" lib/build.f"  >LEN PROC-ARGV+
   s" lib/codesign.f"  >LEN PROC-ARGV+
   s" tools/build-fixpoint.f"  >LEN PROC-ARGV+
   s" tools/build-fixpoint-main.f"  >LEN PROC-ARGV+
   s" --"  >LEN PROC-ARGV+
   s" install"  >LEN PROC-ARGV+ ;

: PERF-SELF-REBUILD ( -- )
   PERF-STUB? if s" self_rebuild" PERF-MEASURE-STUB exit then
   PERF-INSTALL-ARGS
   s" self_rebuild" PERF-MEASURE-ARGV ;

: PERF-AOT-SOURCE$ ( -- ptr u8 n )
   s" : FIB ( i64 -- i64 ) dup 2 < if exit then dup 1 - recurse swap 2 - recurse + ; : MAIN ( -- ) 10 FIB . cr ;" ;

: PERF-PREPARE-AOT ( -- )
   PERF-ROOT-U @ 0= if
      s" hb-llm-perf" TMPDIR-MKDIR PERF-ROOT PERF-ROOT-U PERF-COPY!
      PERF-ROOT$ CLEANUP-TREE+
      PERF-ROOT$ s" perf-main.f" PERF-AOT-SRC JOIN-PATH PERF-AOT-SRC-U !
      PERF-ROOT$ s" perf-main" PERF-AOT-BIN JOIN-PATH PERF-AOT-BIN-U !
   then
   PERF-AOT-SRC$ PERF-AOT-SOURCE$ WRITE-ALL ;

: PERF-BUILD-AOT-ARGS ( -- )
   PROC-ARGV-RESET
   s" --load"  >LEN PROC-ARGV+
   s" lib/errors.f"  >LEN PROC-ARGV+
   s" lib/string.f"  >LEN PROC-ARGV+
   s" lib/fs.f"  >LEN PROC-ARGV+
   s" lib/fs-mutate.f"  >LEN PROC-ARGV+
   s" lib/process.f"  >LEN PROC-ARGV+
   s" lib/process-argv.f"  >LEN PROC-ARGV+
   s" lib/process-env.f"  >LEN PROC-ARGV+
   s" lib/source.f"  >LEN PROC-ARGV+
   s" lib/build.f"  >LEN PROC-ARGV+
   s" lib/codesign.f"  >LEN PROC-ARGV+
   s" tools/build-fixpoint.f"  >LEN PROC-ARGV+
   s" tools/hb-build-lib.f"  >LEN PROC-ARGV+
   s" tools/hb-build.f"  >LEN PROC-ARGV+
   s" --"  >LEN PROC-ARGV+
   PERF-AOT-SRC$  >LEN PROC-ARGV+
   s" -o"  >LEN PROC-ARGV+
   PERF-AOT-BIN$  >LEN PROC-ARGV+ ;

: PERF-HB-BUILD-AOT ( -- )
   PERF-STUB? if s" hb_build_aot" PERF-MEASURE-STUB exit then
   PERF-PREPARE-AOT
   PERF-BUILD-AOT-ARGS
   s" hb_build_aot" PERF-MEASURE-ARGV ;

: PERF-AOT-RUNTIME ( -- )
   PERF-STUB? if s" aot_runtime" PERF-MEASURE-STUB exit then
   s" aot_runtime" PERF-AOT-BIN$ PERF-MEASURE-PATH ;

: PERF-RUN-QUICK ( -- )
   PERF-CHECK-SOLUTIONS
   PERF-FUNCTIONAL-TESTS
   PERF-METRIC-VALIDATOR
   PERF-PROP-SMOKE
   PERF-MICROBENCH-SMOKE ;

: PERF-RUN-FULL ( -- )
   PERF-FULL @ 0= if exit then
   PERF-SELF-REBUILD
   PERF-HB-BUILD-AOT
   PERF-AOT-RUNTIME ;

: PERF-JSON-NAME ( ptr u8 n -- )
   PERF-DQ emit
   type
   PERF-DQ emit ;

: PERF-BEGIN ( -- )
   -1 PERF-FIRST !
   PERF-JSON @ 0 <> if
      PERF-LBRACE emit
      s" schema_version" PERF-JSON-NAME
      PERF-COLON emit
      s" 1" type
      PERF-COMMA emit
      s" bench" PERF-JSON-NAME
      PERF-COLON emit
      s" llm-perf" PERF-JSON-NAME
      PERF-COMMA emit
      s" full" PERF-JSON-NAME
      PERF-COLON emit
      PERF-FULL @ 0 <> if s" true" else s" false" then type
      PERF-COMMA emit
      s" results" PERF-JSON-NAME
      PERF-COLON emit
      PERF-LBRACK emit
      exit
   then
   PERF-FULL @ 0 <> if
      s" llm-perf: mode=full" type cr
   else
      s" llm-perf: mode=quick" type cr
   then ;

: PERF-END ( -- )
   PERF-JSON @ 0 <> if s" ]}" type cr then ;

: PERF-MAIN ( -- )
   CLEANUP-RESET
   0 PERF-ROOT-U !
   PERF-PARSE
   PERF-REQUIRE-HB
   PERF-BEGIN
   PERF-RUN-QUICK
   PERF-RUN-FULL
   PERF-END
   CLEANUP-RUN ;
