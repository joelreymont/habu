\ run-lib.f - checked native benchmark gate runner.
\
\ Load after test/gate-common.f.

64 constant BGR-USAGE-RC

: BGR-USAGE ( -- )
   s" usage: load bench/llm/run-lib.f bench/llm/run.f" BGR-USAGE-RC die ;

: BGR-ARG+ ( ptr u8 n -- )
   >LEN PROC-ARGV+ ;

: BGR-HB ( -- )
   GE-HB-RESET
   s" --load" BGR-ARG+ ;

: BGR-RUN ( ptr u8 n -- )
   GE-HB-RUN ;

: BGR-ERR-STR ( -- )
   s" lib/errors.f" BGR-ARG+
   s" lib/string.f" BGR-ARG+ ;

: BGR-TEST ( -- )
   s" lib/test.f" BGR-ARG+ ;

: BGR-MEMORY ( -- )
   s" lib/memory.f" BGR-ARG+ ;

: BGR-FS ( -- )
   s" lib/fs.f" BGR-ARG+
   s" lib/fs-mutate.f" BGR-ARG+ ;

: BGR-PROC ( -- )
   s" lib/process.f" BGR-ARG+
   s" lib/process-argv.f" BGR-ARG+ ;

: BGR-PROC-ENV ( -- )
   BGR-PROC
   s" lib/process-env.f" BGR-ARG+ ;

: BGR-JSON-TOOLS ( -- )
   s" tools/json.f" BGR-ARG+
   s" tools/argv.f" BGR-ARG+ ;

: BGR-CHECK-ARGS ( -- )
   BGR-HB
   BGR-ERR-STR
   s" lib/fs.f" BGR-ARG+
   s" lib/fs-mutate.f" BGR-ARG+
   s" lib/process.f" BGR-ARG+
   s" lib/process-argv.f" BGR-ARG+
   s" lib/source.f" BGR-ARG+
   s" tools/argv.f" BGR-ARG+
   s" tools/check.f" BGR-ARG+
   s" --" BGR-ARG+ ;

: BGR-CHECK-SOLUTIONS ( -- )
   BGR-CHECK-ARGS
   s" bench/llm/solutions.f" BGR-ARG+
   s" llm reference solution certification" BGR-RUN ;

: BGR-FUNCTIONAL-TESTS ( -- )
   BGR-HB
   s" bench/llm/solutions.f" BGR-ARG+
   s" bench/llm/tests.f" BGR-ARG+
   s" llm reference functional tests" BGR-RUN ;

: BGR-REF-SOLUTIONS ( -- )
   BGR-HB
   BGR-ERR-STR
   s" lib/regex.f" BGR-ARG+
   s" lib/map.f" BGR-ARG+
   s" lib/date.f" BGR-ARG+
   s" lib/time.f" BGR-ARG+
   s" lib/fs.f" BGR-ARG+
   s" bench/llm/ref-solutions.f" BGR-ARG+
   s" llm V2 reference solutions" BGR-RUN ;

: BGR-MANIFEST-AUDIT ( -- )
   BGR-HB
   BGR-ERR-STR
   s" lib/fs.f" BGR-ARG+
   s" bench/llm/manifest.f" BGR-ARG+
   s" bench/llm/manifest-audit.f" BGR-ARG+
   s" bench/llm/manifest-audit-main.f" BGR-ARG+
   s" llm manifest audit CLI" BGR-RUN ;

: BGR-JSON-ROW ( -- )
   BGR-HB
   BGR-ERR-STR BGR-TEST
   BGR-MEMORY
   s" lib/json-write.f" BGR-ARG+
   s" bench/llm/fixture-text.f" BGR-ARG+
   s" bench/llm/json-row.f" BGR-ARG+
   s" bench/llm/json-row-test.f" BGR-ARG+
   s" llm json row helpers" BGR-RUN ;

: BGR-MANIFEST ( -- )
   BGR-HB
   BGR-ERR-STR BGR-TEST
   s" bench/llm/manifest.f" BGR-ARG+
   s" bench/llm/manifest-test.f" BGR-ARG+
   s" llm manifest scanner" BGR-RUN ;

: BGR-MODEL ( -- )
   BGR-HB
   BGR-ERR-STR BGR-TEST
   s" lib/fs.f" BGR-ARG+
   s" bench/llm/manifest.f" BGR-ARG+
   s" bench/llm/model.f" BGR-ARG+
   s" bench/llm/model-test.f" BGR-ARG+
   s" llm model registry" BGR-RUN ;

: BGR-VECTORS ( -- )
   BGR-HB
   BGR-ERR-STR BGR-TEST
   s" bench/llm/manifest.f" BGR-ARG+
   s" bench/llm/vectors.f" BGR-ARG+
   s" bench/llm/vectors-test.f" BGR-ARG+
   s" llm vector parser" BGR-RUN ;

: BGR-FOREIGN-VECTORS ( -- )
   BGR-HB
   BGR-ERR-STR BGR-TEST
   s" bench/llm/manifest.f" BGR-ARG+
   s" bench/llm/vectors.f" BGR-ARG+
   s" bench/llm/foreign-vectors.f" BGR-ARG+
   s" bench/llm/foreign-vectors-test.f" BGR-ARG+
   s" llm foreign vector emitters" BGR-RUN ;

: BGR-ARTIFACTS ( -- )
   BGR-HB
   BGR-ERR-STR BGR-TEST BGR-FS
   s" src/core/sha256.f" BGR-ARG+
   s" bench/llm/artifacts.f" BGR-ARG+
   s" bench/llm/artifacts-test.f" BGR-ARG+
   s" llm artifact hashes" BGR-RUN ;

: BGR-NEGATIVE ( -- )
   BGR-HB
   BGR-ERR-STR BGR-TEST
   s" bench/llm/negative-score.f" BGR-ARG+
   s" bench/llm/negative-score-test.f" BGR-ARG+
   s" llm negative scorer" BGR-RUN ;

: BGR-GRADE ( -- )
   BGR-HB
   BGR-ERR-STR BGR-TEST BGR-FS BGR-PROC
   s" bench/llm/grade.f" BGR-ARG+
   s" bench/llm/grade-test.f" BGR-ARG+
   s" llm native grader" BGR-RUN ;

: BGR-VALIDATE-RESULTS ( -- )
   BGR-HB
   s" tools/date.f" BGR-ARG+
   s" tools/lint/lib.f" BGR-ARG+
   s" tools/json.f" BGR-ARG+
   s" tools/argv.f" BGR-ARG+
   s" bench/llm/validate-results.f" BGR-ARG+
   s" llm metric validator" BGR-RUN ;

: BGR-VALIDATOR-TEST ( -- )
   BGR-HB
   BGR-ERR-STR BGR-TEST BGR-FS BGR-PROC-ENV
   BGR-MEMORY
   s" lib/json-write.f" BGR-ARG+
   s" bench/llm/fixture-text.f" BGR-ARG+
   s" bench/llm/manifest.f" BGR-ARG+
   s" bench/llm/validate-results-test.f" BGR-ARG+
   s" llm validator positive fixtures" BGR-RUN ;

: BGR-EXPANDED-RUNNER ( -- )
   BGR-HB
   BGR-ERR-STR BGR-TEST BGR-FS BGR-PROC-ENV
   BGR-MEMORY
   s" lib/json-write.f" BGR-ARG+
   s" bench/llm/fixture-text.f" BGR-ARG+
   s" bench/llm/run-expanded-bench-test.f" BGR-ARG+
   s" llm expanded runner" BGR-RUN ;

: BGR-REPORT ( -- )
   BGR-HB
   BGR-ERR-STR BGR-TEST
   BGR-MEMORY
   s" lib/json-write.f" BGR-ARG+
   BGR-FS BGR-PROC
   s" bench/llm/report-test.f" BGR-ARG+
   s" llm report" BGR-RUN ;

: BGR-EXPANDED-REPORT ( -- )
   BGR-HB
   BGR-ERR-STR BGR-TEST
   BGR-MEMORY
   s" lib/json-write.f" BGR-ARG+
   BGR-FS BGR-PROC
   s" bench/llm/expanded-report-test.f" BGR-ARG+
   s" llm expanded report" BGR-RUN ;

: BGR-RUN-REFERENCE ( -- )
   BGR-MANIFEST-AUDIT
   BGR-CHECK-SOLUTIONS
   BGR-FUNCTIONAL-TESTS
   BGR-REF-SOLUTIONS ;

: BGR-RUN-FIXTURES ( -- )
   BGR-JSON-ROW
   BGR-MANIFEST
   BGR-MODEL
   BGR-VECTORS
   BGR-FOREIGN-VECTORS
   BGR-ARTIFACTS
   BGR-NEGATIVE
   BGR-GRADE
   BGR-VALIDATE-RESULTS
   BGR-VALIDATOR-TEST
   BGR-EXPANDED-RUNNER
   BGR-REPORT
   BGR-EXPANDED-REPORT ;

: BGR-MAIN ( -- )
   SCRIPT-ARGC 0 <> if BGR-USAGE then
   GT-RESET
   BGR-RUN-REFERENCE
   BGR-RUN-FIXTURES
   s" PASS: bench/llm native gate" type cr ;
