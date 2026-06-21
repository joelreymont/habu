\ gate-stdlib.f - checked runner for the default gate lint/stdlib phase.
\
\ Load after lib/errors.f, lib/string.f, lib/fs.f, lib/fs-mutate.f,
\ lib/process.f, lib/process-argv.f, lib/process-env.f, and
\ lib/test-runner.f.

$40000 constant GS-SRC-CAP
120000 constant GS-TIMEOUT-MS

create GS-SRC-BUF GS-SRC-CAP allot

variable GS-SRC-U
variable GS-RD

: GS-RUN-ENV ( ptr u8 n n -- ) {: path:ptr pathu timeout :}
   PROC-ENV-INHERIT-MISSING
   path pathu GT-OUT-BUF GT-OUT-CAP GT-ERR-BUF GT-ERR-CAP timeout
   RUN-ARGV-ENV-CAPTURE
   GT-OUTCOME-CODE !
   PROC-OUTCOME-EXIT GT-OUTCOME-KIND !
   GT-ERR-U !
   GT-OUT-U ! ;

: GS-RUN-STDIN ( ptr u8 n ptr u8 n n -- ) {: path:ptr pathu in:ptr inu timeout :}
   PROC-ENV-INHERIT-MISSING
   path pathu in inu GT-OUT-BUF GT-OUT-CAP GT-ERR-BUF GT-ERR-CAP timeout
   RUN-ARGV-ENV-STDIN-CAPTURE
   GT-OUTCOME-CODE !
   PROC-OUTCOME-EXIT GT-OUTCOME-KIND !
   GT-ERR-U !
   GT-OUT-U ! ;

: GS-FAIL ( ptr u8 n -- ) {: label:ptr labelu :}
   s" FAIL: " type label labelu type cr
   s" rc: " type GT-RC@ . cr
   GT-OUT$ type
   GT-ERR$ type
   s" gate stdlib phase failed" 1 die ;

: GS-EXPECT-OK ( ptr u8 n -- ) {: label:ptr labelu :}
   GT-RC@ 0 <> if label labelu GS-FAIL then ;

: GS-ARG+ ( ptr u8 n -- )
   PROC-ARGV+ ;

: GS-HB ( -- )
   PROC-ARGV-RESET
   s" --load" GS-ARG+ ;

: GS-HB-RUN ( ptr u8 n -- ) {: label:ptr labelu :}
   s" bin/hb" GS-TIMEOUT-MS GS-RUN-ENV
   label labelu GS-EXPECT-OK ;

: GS-HB-RUN-STDIN ( ptr u8 n ptr u8 n -- ) {: in:ptr inu label:ptr labelu :}
   s" bin/hb" in inu GS-TIMEOUT-MS GS-RUN-STDIN
   label labelu GS-EXPECT-OK ;

: GS-SRC-RESET ( -- )
   0 GS-SRC-U ! ;

: GS-SRC+ ( ptr u8 n -- ) {: path:ptr pathu :}
   path pathu GS-SRC-BUF GS-SRC-U @ + GS-SRC-CAP GS-SRC-U @ -
   READ-ALL GS-RD !
   GS-SRC-U @ GS-RD @ + GS-SRC-U ! ;

: GS-CHECK-ARGV ( -- )
   PROC-ARGV-RESET
   s" --load" GS-ARG+
   s" lib/errors.f" GS-ARG+
   s" lib/string.f" GS-ARG+
   s" lib/fs.f" GS-ARG+
   s" lib/fs-mutate.f" GS-ARG+
   s" lib/process.f" GS-ARG+
   s" lib/process-argv.f" GS-ARG+
   s" lib/source.f" GS-ARG+
   s" tools/argv.f" GS-ARG+
   s" tools/check.f" GS-ARG+
   s" --" GS-ARG+ ;

: GS-CHECK-RUN ( ptr u8 n -- ) {: label:ptr labelu :}
   GS-CHECK-ARGV
   s" bin/hb" GS-SRC-BUF GS-SRC-U @ GS-TIMEOUT-MS GS-RUN-STDIN
   label labelu GS-EXPECT-OK ;

: GS-SRC-COMMON-PROC ( -- )
   GS-SRC-RESET
   s" lib/errors.f" GS-SRC+
   s" lib/string.f" GS-SRC+
   s" lib/test.f" GS-SRC+
   s" lib/fs.f" GS-SRC+
   s" lib/fs-mutate.f" GS-SRC+
   s" lib/process.f" GS-SRC+
   s" lib/process-argv.f" GS-SRC+ ;

: GS-CHECK-COMMON-PROC ( ptr u8 n ptr u8 n -- ) {: file:ptr fileu label:ptr labelu :}
   GS-SRC-COMMON-PROC
   file fileu GS-SRC+
   label labelu GS-CHECK-RUN ;

: GS-CHECK-JSON-WRITE ( ptr u8 n ptr u8 n -- ) {: file:ptr fileu label:ptr labelu :}
   GS-SRC-RESET
   s" lib/errors.f" GS-SRC+
   s" lib/string.f" GS-SRC+
   s" lib/test.f" GS-SRC+
   s" lib/json-write.f" GS-SRC+
   file fileu GS-SRC+
   label labelu GS-CHECK-RUN ;

: GS-CHECK-LINTS ( -- )
   GS-HB s" tools/lint/lib.f" GS-ARG+ s" tools/lint/shadow-lint.f" GS-ARG+
   s" shadow-lint" GS-HB-RUN
   GS-HB s" tools/lint/lib.f" GS-ARG+ s" tools/lint/clobber-lint.f" GS-ARG+
   s" clobber-lint" GS-HB-RUN
   GS-HB s" tools/lint/lib.f" GS-ARG+ s" tools/argv.f" GS-ARG+ s" tools/repl-lint.f" GS-ARG+
   s" repl-lint" GS-HB-RUN
   GS-HB s" tools/date.f" GS-ARG+ s" tools/lint/lib.f" GS-ARG+ s" tools/fs.f" GS-ARG+ s" tools/argv.f" GS-ARG+ s" tools/trust-lint.f" GS-ARG+
   s" trust-lint" GS-HB-RUN
   GS-HB s" tools/date.f" GS-ARG+ s" tools/lint/lib.f" GS-ARG+ s" tools/fs.f" GS-ARG+ s" tools/argv.f" GS-ARG+ s" tools/stale-status-lint.f" GS-ARG+
   s" stale-status-lint" GS-HB-RUN
   GS-HB s" tools/lint/lib.f" GS-ARG+ s" tools/fs.f" GS-ARG+ s" tools/host-lint.f" GS-ARG+
   s" host-lint" GS-HB-RUN
   GS-HB s" tools/lint/lib.f" GS-ARG+ s" tools/parallel-agent-lint.f" GS-ARG+
   s" parallel-agent-lint" GS-HB-RUN
   GS-HB s" tools/lint/lib.f" GS-ARG+ s" tools/filemap-lint.f" GS-ARG+
   s" filemap-lint" GS-HB-RUN ;

: GS-CHECK-TOOL-FIXTURES ( -- )
   s" tools/repl-lint-test.f" s" repl-lint fixture check" GS-CHECK-COMMON-PROC
   s" tools/diag-origin-test.f" s" diag-origin fixture check" GS-CHECK-COMMON-PROC
   s" tools/check-all-errors-test.f" s" check-all-errors fixture check" GS-CHECK-COMMON-PROC
   s" tools/aot-lint-test.f" s" aot-lint fixture check" GS-CHECK-COMMON-PROC
   s" tools/signature-lint-test.f" s" signature-lint fixture check" GS-CHECK-COMMON-PROC
   s" tools/public-signatures-test.f" s" public-signatures fixture check" GS-CHECK-COMMON-PROC
   GS-SRC-RESET
   s" tools/aot-call-report.f" GS-SRC+
   s" tools/aot-call-report-test.f" GS-SRC+
   s" aot-call-report fixture check" GS-CHECK-RUN
   s" tools/trust-lint-test.f" s" trust-lint fixture check" GS-CHECK-COMMON-PROC
   s" tools/stale-status-lint-test.f" s" stale-status-lint fixture check" GS-CHECK-COMMON-PROC
   s" tools/checked-boundary-lint-test.f" s" checked-boundary-lint fixture check" GS-CHECK-COMMON-PROC
   s" tools/bundle-lib-test.f" s" bundle-lib fixture check" GS-CHECK-COMMON-PROC
   s" tools/examples-test.f" s" examples fixture check" GS-CHECK-COMMON-PROC
   s" tools/repair-schema-doc-test.f" s" repair diagnostic schema doc check" GS-CHECK-COMMON-PROC
   s" tools/repair-packet-test.f" s" repair packet fixture check" GS-CHECK-COMMON-PROC
   s" tools/json-only-test.f" s" json-only tool check" GS-CHECK-COMMON-PROC
   s" tools/check-test.f" s" native check runner fixture check" GS-CHECK-COMMON-PROC ;

: GS-RUN-TOOL-FIXTURES ( -- )
   GS-HB s" lib/errors.f" GS-ARG+ s" lib/string.f" GS-ARG+ s" lib/test.f" GS-ARG+ s" lib/fs.f" GS-ARG+ s" lib/fs-mutate.f" GS-ARG+ s" lib/process.f" GS-ARG+ s" lib/process-argv.f" GS-ARG+ s" tools/repl-lint-test.f" GS-ARG+
   s" repl-lint fixtures" GS-HB-RUN
   GS-HB s" lib/errors.f" GS-ARG+ s" lib/string.f" GS-ARG+ s" lib/test.f" GS-ARG+ s" lib/fs.f" GS-ARG+ s" lib/fs-mutate.f" GS-ARG+ s" lib/process.f" GS-ARG+ s" lib/process-argv.f" GS-ARG+ s" tools/diag-origin-test.f" GS-ARG+
   s" diag-origin fixtures" GS-HB-RUN
   GS-HB s" lib/errors.f" GS-ARG+ s" lib/string.f" GS-ARG+ s" lib/test.f" GS-ARG+ s" lib/fs.f" GS-ARG+ s" lib/fs-mutate.f" GS-ARG+ s" lib/process.f" GS-ARG+ s" lib/process-argv.f" GS-ARG+ s" tools/check-all-errors-test.f" GS-ARG+
   s" check-all-errors fixtures" GS-HB-RUN
   GS-HB s" lib/errors.f" GS-ARG+ s" lib/string.f" GS-ARG+ s" lib/test.f" GS-ARG+ s" lib/fs.f" GS-ARG+ s" lib/fs-mutate.f" GS-ARG+ s" lib/process.f" GS-ARG+ s" lib/process-argv.f" GS-ARG+ s" tools/aot-lint-test.f" GS-ARG+
   s" aot-lint fixtures" GS-HB-RUN
   GS-HB s" lib/errors.f" GS-ARG+ s" lib/string.f" GS-ARG+ s" lib/test.f" GS-ARG+ s" lib/fs.f" GS-ARG+ s" lib/fs-mutate.f" GS-ARG+ s" lib/process.f" GS-ARG+ s" lib/process-argv.f" GS-ARG+ s" tools/signature-lint-test.f" GS-ARG+
   s" signature-lint fixtures" GS-HB-RUN
   GS-HB s" lib/errors.f" GS-ARG+ s" lib/string.f" GS-ARG+ s" lib/test.f" GS-ARG+ s" lib/fs.f" GS-ARG+ s" lib/fs-mutate.f" GS-ARG+ s" lib/process.f" GS-ARG+ s" lib/process-argv.f" GS-ARG+ s" tools/public-signatures-test.f" GS-ARG+
   s" public-signatures fixtures" GS-HB-RUN
   GS-HB s" lib/errors.f" GS-ARG+ s" lib/string.f" GS-ARG+ s" lib/fs.f" GS-ARG+ s" lib/process.f" GS-ARG+ s" lib/process-argv.f" GS-ARG+ s" tools/lint/lib.f" GS-ARG+ s" tools/stdlib-manifest-test.f" GS-ARG+
   s" stdlib manifest" GS-HB-RUN
   GS-HB s" tools/aot-call-report.f" GS-ARG+ s" tools/aot-call-report-test.f" GS-ARG+
   s" aot-call-report fixtures" GS-HB-RUN
   GS-HB s" lib/errors.f" GS-ARG+ s" lib/string.f" GS-ARG+ s" lib/test.f" GS-ARG+ s" lib/fs.f" GS-ARG+ s" lib/fs-mutate.f" GS-ARG+ s" lib/process.f" GS-ARG+ s" lib/process-argv.f" GS-ARG+ s" tools/trust-lint-test.f" GS-ARG+
   s" trust-lint fixtures" GS-HB-RUN
   GS-HB s" lib/errors.f" GS-ARG+ s" lib/string.f" GS-ARG+ s" lib/test.f" GS-ARG+ s" lib/fs.f" GS-ARG+ s" lib/fs-mutate.f" GS-ARG+ s" lib/process.f" GS-ARG+ s" lib/process-argv.f" GS-ARG+ s" tools/stale-status-lint-test.f" GS-ARG+
   s" stale-status-lint fixtures" GS-HB-RUN
   GS-HB s" lib/errors.f" GS-ARG+ s" lib/string.f" GS-ARG+ s" lib/test.f" GS-ARG+ s" lib/fs.f" GS-ARG+ s" lib/fs-mutate.f" GS-ARG+ s" lib/process.f" GS-ARG+ s" lib/process-argv.f" GS-ARG+ s" tools/checked-boundary-lint-test.f" GS-ARG+
   s" checked-boundary-lint" GS-HB-RUN
   GS-HB s" lib/errors.f" GS-ARG+ s" lib/string.f" GS-ARG+ s" lib/test.f" GS-ARG+ s" lib/fs.f" GS-ARG+ s" lib/fs-mutate.f" GS-ARG+ s" lib/process.f" GS-ARG+ s" lib/process-argv.f" GS-ARG+ s" tools/imgdump.f" GS-ARG+ s" tools/imgdump-test.f" GS-ARG+
   s" imgdump compare" GS-HB-RUN ;

: GS-RUN-LLM-FIXTURES ( -- )
   GS-HB s" lib/errors.f" GS-ARG+ s" lib/string.f" GS-ARG+ s" lib/test.f" GS-ARG+ s" lib/fs.f" GS-ARG+ s" bench/llm/manifest.f" GS-ARG+ s" bench/llm/model.f" GS-ARG+ s" bench/llm/model-test.f" GS-ARG+
   s" llm model registry" GS-HB-RUN
   GS-HB s" lib/errors.f" GS-ARG+ s" lib/string.f" GS-ARG+ s" lib/test.f" GS-ARG+ s" lib/fs.f" GS-ARG+ s" lib/process.f" GS-ARG+ s" lib/process-argv.f" GS-ARG+ s" lib/process-env.f" GS-ARG+ s" tools/json.f" GS-ARG+ s" tools/argv.f" GS-ARG+ s" bench/llm/manifest.f" GS-ARG+ s" bench/llm/model.f" GS-ARG+ s" bench/llm/parse-resp-lib.f" GS-ARG+ s" bench/llm/model-run.f" GS-ARG+ s" bench/llm/model-run-test.f" GS-ARG+
   s" llm model runner" GS-HB-RUN
   GS-HB s" lib/errors.f" GS-ARG+ s" lib/string.f" GS-ARG+ s" lib/test.f" GS-ARG+ s" lib/fs.f" GS-ARG+ s" bench/llm/manifest.f" GS-ARG+ s" bench/llm/manifest-audit.f" GS-ARG+ s" bench/llm/manifest-audit-test.f" GS-ARG+
   s" llm manifest audit" GS-HB-RUN
   GS-HB s" lib/errors.f" GS-ARG+ s" lib/string.f" GS-ARG+ s" lib/test.f" GS-ARG+ s" bench/llm/manifest.f" GS-ARG+ s" bench/llm/vectors.f" GS-ARG+ s" bench/llm/vectors-test.f" GS-ARG+
   s" llm vector parser" GS-HB-RUN
   GS-HB s" lib/errors.f" GS-ARG+ s" lib/string.f" GS-ARG+ s" lib/test.f" GS-ARG+ s" lib/fs.f" GS-ARG+ s" lib/fs-mutate.f" GS-ARG+ s" src/core/sha256.f" GS-ARG+ s" bench/llm/artifacts.f" GS-ARG+ s" bench/llm/artifacts-test.f" GS-ARG+
   s" llm artifact hashes" GS-HB-RUN
   GS-HB s" lib/errors.f" GS-ARG+ s" lib/string.f" GS-ARG+ s" lib/test.f" GS-ARG+ s" lib/fs.f" GS-ARG+ s" lib/fs-mutate.f" GS-ARG+ s" lib/json-write.f" GS-ARG+ s" src/core/sha256.f" GS-ARG+ s" bench/llm/live-row.f" GS-ARG+ s" bench/llm/live-row-test.f" GS-ARG+
   s" llm live row emitter" GS-HB-RUN
   GS-HB s" lib/errors.f" GS-ARG+ s" lib/string.f" GS-ARG+ s" lib/test.f" GS-ARG+ s" lib/json-write.f" GS-ARG+ s" bench/llm/fixture-text.f" GS-ARG+ s" bench/llm/fixture-text-test.f" GS-ARG+
   s" llm fixture text helpers" GS-HB-RUN
   GS-HB s" lib/errors.f" GS-ARG+ s" lib/string.f" GS-ARG+ s" lib/test.f" GS-ARG+ s" lib/json-write.f" GS-ARG+ s" bench/llm/fixture-text.f" GS-ARG+ s" bench/llm/json-row.f" GS-ARG+ s" bench/llm/json-row-test.f" GS-ARG+
   s" llm json row helpers" GS-HB-RUN
   GS-HB s" lib/errors.f" GS-ARG+ s" lib/string.f" GS-ARG+ s" lib/test.f" GS-ARG+ s" bench/llm/negative-score.f" GS-ARG+ s" bench/llm/negative-score-test.f" GS-ARG+
   s" llm negative scorer" GS-HB-RUN
   GS-HB s" lib/errors.f" GS-ARG+ s" lib/string.f" GS-ARG+ s" lib/test.f" GS-ARG+ s" lib/fs.f" GS-ARG+ s" lib/fs-mutate.f" GS-ARG+ s" lib/process.f" GS-ARG+ s" lib/process-argv.f" GS-ARG+ s" bench/llm/grade.f" GS-ARG+ s" bench/llm/grade-test.f" GS-ARG+
   s" llm native grader" GS-HB-RUN ;

: GS-CHECK-LLM-FIXTURES ( -- )
   GS-SRC-RESET
   s" lib/errors.f" GS-SRC+ s" lib/string.f" GS-SRC+ s" lib/test.f" GS-SRC+ s" lib/fs.f" GS-SRC+ s" bench/llm/manifest.f" GS-SRC+ s" bench/llm/model.f" GS-SRC+ s" bench/llm/model-test.f" GS-SRC+
   s" llm model registry check" GS-CHECK-RUN
   GS-SRC-RESET
   s" lib/errors.f" GS-SRC+ s" lib/string.f" GS-SRC+ s" lib/test.f" GS-SRC+ s" lib/fs.f" GS-SRC+ s" bench/llm/manifest.f" GS-SRC+ s" bench/llm/manifest-audit.f" GS-SRC+ s" bench/llm/manifest-audit-test.f" GS-SRC+
   s" llm manifest audit check" GS-CHECK-RUN
   GS-SRC-RESET
   s" lib/errors.f" GS-SRC+ s" lib/string.f" GS-SRC+ s" lib/test.f" GS-SRC+ s" bench/llm/manifest.f" GS-SRC+ s" bench/llm/vectors.f" GS-SRC+ s" bench/llm/vectors-test.f" GS-SRC+
   s" llm vector parser check" GS-CHECK-RUN
   GS-SRC-RESET
   s" lib/errors.f" GS-SRC+ s" lib/string.f" GS-SRC+ s" lib/test.f" GS-SRC+ s" lib/fs.f" GS-SRC+ s" lib/fs-mutate.f" GS-SRC+ s" src/core/sha256.f" GS-SRC+ s" bench/llm/artifacts.f" GS-SRC+ s" bench/llm/artifacts-test.f" GS-SRC+
   s" llm artifact hash check" GS-CHECK-RUN
   GS-SRC-RESET
   s" lib/errors.f" GS-SRC+ s" lib/string.f" GS-SRC+ s" lib/test.f" GS-SRC+ s" lib/fs.f" GS-SRC+ s" lib/fs-mutate.f" GS-SRC+ s" lib/json-write.f" GS-SRC+ s" src/core/sha256.f" GS-SRC+ s" bench/llm/live-row.f" GS-SRC+ s" bench/llm/live-row-test.f" GS-SRC+
   s" llm live row emitter check" GS-CHECK-RUN
   GS-SRC-RESET
   s" lib/errors.f" GS-SRC+ s" lib/string.f" GS-SRC+ s" lib/test.f" GS-SRC+ s" lib/json-write.f" GS-SRC+ s" bench/llm/fixture-text.f" GS-SRC+ s" bench/llm/fixture-text-test.f" GS-SRC+
   s" llm fixture text helper check" GS-CHECK-RUN
   GS-SRC-RESET
   s" lib/errors.f" GS-SRC+ s" lib/string.f" GS-SRC+ s" lib/test.f" GS-SRC+ s" lib/json-write.f" GS-SRC+ s" bench/llm/fixture-text.f" GS-SRC+ s" bench/llm/json-row.f" GS-SRC+ s" bench/llm/json-row-test.f" GS-SRC+
   s" llm json row helper check" GS-CHECK-RUN
   GS-SRC-RESET
   s" lib/errors.f" GS-SRC+ s" lib/string.f" GS-SRC+ s" lib/test.f" GS-SRC+ s" bench/llm/negative-score.f" GS-SRC+ s" bench/llm/negative-score-test.f" GS-SRC+
   s" llm negative scorer check" GS-CHECK-RUN
   GS-SRC-RESET
   s" lib/errors.f" GS-SRC+ s" lib/string.f" GS-SRC+ s" lib/test.f" GS-SRC+ s" lib/fs.f" GS-SRC+ s" lib/fs-mutate.f" GS-SRC+ s" lib/process.f" GS-SRC+ s" lib/process-argv.f" GS-SRC+ s" bench/llm/grade.f" GS-SRC+ s" bench/llm/grade-test.f" GS-SRC+
   s" llm native grader check" GS-CHECK-RUN ;

: GS-RUN-LLM-DRIVERS ( -- )
   GS-HB s" lib/errors.f" GS-ARG+ s" lib/string.f" GS-ARG+ s" lib/test.f" GS-ARG+ s" lib/fs.f" GS-ARG+ s" lib/fs-mutate.f" GS-ARG+ s" lib/process.f" GS-ARG+ s" lib/process-argv.f" GS-ARG+ s" lib/process-env.f" GS-ARG+ s" tools/argv.f" GS-ARG+ s" tools/json.f" GS-ARG+ s" bench/llm/manifest.f" GS-ARG+ s" bench/llm/model.f" GS-ARG+ s" bench/llm/parse-resp-lib.f" GS-ARG+ s" bench/llm/model-run.f" GS-ARG+ s" bench/llm/vectors.f" GS-ARG+ s" lib/json-write.f" GS-ARG+ s" src/core/sha256.f" GS-ARG+ s" bench/llm/live-row.f" GS-ARG+ s" bench/llm/drive-stdlib-lib.f" GS-ARG+ s" bench/llm/driver-test-helpers.f" GS-ARG+ s" bench/llm/drive-stdlib-test.f" GS-ARG+
   s" llm stdlib stack driver" GS-HB-RUN
   GS-HB s" lib/errors.f" GS-ARG+ s" lib/string.f" GS-ARG+ s" lib/test.f" GS-ARG+ s" lib/fs.f" GS-ARG+ s" lib/fs-mutate.f" GS-ARG+ s" lib/process.f" GS-ARG+ s" lib/process-argv.f" GS-ARG+ s" lib/process-env.f" GS-ARG+ s" tools/argv.f" GS-ARG+ s" tools/json.f" GS-ARG+ s" bench/llm/manifest.f" GS-ARG+ s" bench/llm/model.f" GS-ARG+ s" bench/llm/parse-resp-lib.f" GS-ARG+ s" bench/llm/model-run.f" GS-ARG+ s" bench/llm/vectors.f" GS-ARG+ s" lib/json-write.f" GS-ARG+ s" src/core/sha256.f" GS-ARG+ s" bench/llm/live-row.f" GS-ARG+ s" bench/llm/drive-stdlib-lib.f" GS-ARG+ s" bench/llm/driver-test-helpers.f" GS-ARG+ s" bench/llm/drive-regex-negative-lib.f" GS-ARG+ s" bench/llm/drive-regex-negative-test.f" GS-ARG+
   s" llm stdlib regex negative driver" GS-HB-RUN
   GS-HB s" lib/errors.f" GS-ARG+ s" lib/string.f" GS-ARG+ s" lib/test.f" GS-ARG+ s" lib/fs.f" GS-ARG+ s" lib/fs-mutate.f" GS-ARG+ s" lib/process.f" GS-ARG+ s" lib/process-argv.f" GS-ARG+ s" lib/process-env.f" GS-ARG+ s" tools/argv.f" GS-ARG+ s" tools/json.f" GS-ARG+ s" bench/llm/manifest.f" GS-ARG+ s" bench/llm/model.f" GS-ARG+ s" bench/llm/parse-resp-lib.f" GS-ARG+ s" bench/llm/model-run.f" GS-ARG+ s" bench/llm/vectors.f" GS-ARG+ s" lib/json-write.f" GS-ARG+ s" src/core/sha256.f" GS-ARG+ s" bench/llm/live-row.f" GS-ARG+ s" bench/llm/drive-stdlib-lib.f" GS-ARG+ s" bench/llm/driver-test-helpers.f" GS-ARG+ s" bench/llm/driver-fixture-helpers.f" GS-ARG+ s" bench/llm/drive-file-lib.f" GS-ARG+ s" bench/llm/drive-file-test.f" GS-ARG+
   s" llm stdlib file driver" GS-HB-RUN
   GS-HB s" lib/errors.f" GS-ARG+ s" lib/string.f" GS-ARG+ s" lib/test.f" GS-ARG+ s" lib/fs.f" GS-ARG+ s" lib/fs-mutate.f" GS-ARG+ s" lib/process.f" GS-ARG+ s" lib/process-argv.f" GS-ARG+ s" lib/process-env.f" GS-ARG+ s" tools/argv.f" GS-ARG+ s" tools/json.f" GS-ARG+ s" bench/llm/manifest.f" GS-ARG+ s" bench/llm/model.f" GS-ARG+ s" bench/llm/parse-resp-lib.f" GS-ARG+ s" bench/llm/model-run.f" GS-ARG+ s" bench/llm/vectors.f" GS-ARG+ s" lib/json-write.f" GS-ARG+ s" src/core/sha256.f" GS-ARG+ s" bench/llm/live-row.f" GS-ARG+ s" bench/llm/drive-stdlib-lib.f" GS-ARG+ s" bench/llm/driver-test-helpers.f" GS-ARG+ s" bench/llm/driver-fixture-helpers.f" GS-ARG+ s" bench/llm/drive-process-lib.f" GS-ARG+ s" bench/llm/drive-process-test.f" GS-ARG+
   s" llm stdlib process driver" GS-HB-RUN
   GS-HB s" lib/errors.f" GS-ARG+ s" lib/string.f" GS-ARG+ s" lib/test.f" GS-ARG+ s" lib/fs.f" GS-ARG+ s" lib/fs-mutate.f" GS-ARG+ s" lib/process.f" GS-ARG+ s" lib/process-argv.f" GS-ARG+ s" lib/process-env.f" GS-ARG+ s" tools/argv.f" GS-ARG+ s" tools/json.f" GS-ARG+ s" bench/llm/manifest.f" GS-ARG+ s" bench/llm/model.f" GS-ARG+ s" bench/llm/parse-resp-lib.f" GS-ARG+ s" bench/llm/model-run.f" GS-ARG+ s" bench/llm/vectors.f" GS-ARG+ s" lib/json-write.f" GS-ARG+ s" src/core/sha256.f" GS-ARG+ s" bench/llm/live-row.f" GS-ARG+ s" bench/llm/drive-stdlib-lib.f" GS-ARG+ s" bench/llm/driver-test-helpers.f" GS-ARG+ s" bench/llm/driver-token-helpers.f" GS-ARG+ s" bench/llm/driver-fixture-helpers.f" GS-ARG+ s" bench/llm/drive-property-lib.f" GS-ARG+ s" bench/llm/drive-property-test.f" GS-ARG+
   s" llm stdlib property driver" GS-HB-RUN
   GS-HB s" lib/errors.f" GS-ARG+ s" lib/string.f" GS-ARG+ s" lib/test.f" GS-ARG+ s" lib/fs.f" GS-ARG+ s" lib/fs-mutate.f" GS-ARG+ s" lib/process.f" GS-ARG+ s" lib/process-argv.f" GS-ARG+ s" lib/process-env.f" GS-ARG+ s" tools/argv.f" GS-ARG+ s" tools/json.f" GS-ARG+ s" bench/llm/manifest.f" GS-ARG+ s" bench/llm/model.f" GS-ARG+ s" bench/llm/parse-resp-lib.f" GS-ARG+ s" bench/llm/model-run.f" GS-ARG+ s" bench/llm/vectors.f" GS-ARG+ s" lib/json-write.f" GS-ARG+ s" src/core/sha256.f" GS-ARG+ s" bench/llm/live-row.f" GS-ARG+ s" bench/llm/drive-stdlib-lib.f" GS-ARG+ s" bench/llm/driver-test-helpers.f" GS-ARG+ s" bench/llm/driver-token-helpers.f" GS-ARG+ s" bench/llm/driver-fixture-helpers.f" GS-ARG+ s" bench/llm/drive-build-lib.f" GS-ARG+ s" bench/llm/drive-build-test.f" GS-ARG+
   s" llm stdlib build driver" GS-HB-RUN
   GS-HB s" lib/errors.f" GS-ARG+ s" lib/string.f" GS-ARG+ s" lib/test.f" GS-ARG+ s" lib/fs.f" GS-ARG+ s" lib/fs-mutate.f" GS-ARG+ s" lib/process.f" GS-ARG+ s" lib/process-argv.f" GS-ARG+ s" lib/process-env.f" GS-ARG+ s" tools/argv.f" GS-ARG+ s" tools/json.f" GS-ARG+ s" bench/llm/manifest.f" GS-ARG+ s" bench/llm/model.f" GS-ARG+ s" bench/llm/parse-resp-lib.f" GS-ARG+ s" bench/llm/model-run.f" GS-ARG+ s" bench/llm/vectors.f" GS-ARG+ s" lib/json-write.f" GS-ARG+ s" src/core/sha256.f" GS-ARG+ s" bench/llm/live-row.f" GS-ARG+ s" bench/llm/drive-stdlib-lib.f" GS-ARG+ s" bench/llm/driver-test-helpers.f" GS-ARG+ s" bench/llm/driver-token-helpers.f" GS-ARG+ s" bench/llm/drive-aot-lib.f" GS-ARG+ s" bench/llm/drive-aot-test.f" GS-ARG+
   s" llm AOT driver" GS-HB-RUN
   GS-HB s" lib/errors.f" GS-ARG+ s" lib/string.f" GS-ARG+ s" lib/test.f" GS-ARG+ s" lib/fs.f" GS-ARG+ s" lib/fs-mutate.f" GS-ARG+ s" lib/process.f" GS-ARG+ s" lib/process-argv.f" GS-ARG+ s" lib/process-env.f" GS-ARG+ s" lib/json-write.f" GS-ARG+ s" bench/llm/fixture-text.f" GS-ARG+ s" bench/llm/run-expanded-bench-test.f" GS-ARG+
   s" llm expanded runner" GS-HB-RUN
   GS-HB s" lib/errors.f" GS-ARG+ s" lib/string.f" GS-ARG+ s" lib/test.f" GS-ARG+ s" lib/json-write.f" GS-ARG+ s" lib/fs.f" GS-ARG+ s" lib/fs-mutate.f" GS-ARG+ s" lib/process.f" GS-ARG+ s" lib/process-argv.f" GS-ARG+ s" bench/llm/expanded-report-test.f" GS-ARG+
   s" llm expanded report" GS-HB-RUN ;

: GS-CHECK-EXPANDED-REPORT ( -- )
   GS-SRC-RESET
   s" lib/errors.f" GS-SRC+ s" lib/string.f" GS-SRC+ s" lib/test.f" GS-SRC+ s" lib/json-write.f" GS-SRC+ s" lib/fs.f" GS-SRC+ s" lib/fs-mutate.f" GS-SRC+ s" lib/process.f" GS-SRC+ s" lib/process-argv.f" GS-SRC+ s" bench/llm/expanded-report-test.f" GS-SRC+
   s" llm expanded report check" GS-CHECK-RUN ;

: GS-RUN-STDLIB ( -- )
   GS-HB s" lib/errors.f" GS-ARG+ s" lib/string.f" GS-ARG+ s" lib/string-test.f" GS-ARG+
   s" string helpers" GS-HB-RUN
   GS-HB s" lib/errors.f" GS-ARG+ s" lib/string.f" GS-ARG+ s" lib/test.f" GS-ARG+ s" lib/json-write.f" GS-ARG+ s" lib/json-write-test.f" GS-ARG+
   s" json writer helpers" GS-HB-RUN
   GS-HB s" lib/errors.f" GS-ARG+ s" lib/string.f" GS-ARG+ s" lib/test.f" GS-ARG+ s" lib/fs.f" GS-ARG+ s" lib/fs-mutate.f" GS-ARG+ s" lib/process.f" GS-ARG+ s" lib/process-argv.f" GS-ARG+ s" lib/test-runner.f" GS-ARG+ s" lib/test-runner-test.f" GS-ARG+
   s" test runner helpers" GS-HB-RUN
   GS-HB s" lib/errors.f" GS-ARG+ s" lib/array.f" GS-ARG+ s" lib/array-test.f" GS-ARG+
   s" array helpers" GS-HB-RUN
   GS-HB s" lib/errors.f" GS-ARG+ s" lib/test.f" GS-ARG+ s" lib/array.f" GS-ARG+ s" lib/table.f" GS-ARG+ s" lib/table-test.f" GS-ARG+
   s" table stdlib" GS-HB-RUN
   GS-HB s" lib/errors.f" GS-ARG+ s" lib/string.f" GS-ARG+ s" lib/test.f" GS-ARG+ s" lib/regex.f" GS-ARG+ s" lib/regex-test.f" GS-ARG+
   s" regex stdlib" GS-HB-RUN
   GS-HB s" lib/errors.f" GS-ARG+ s" lib/string.f" GS-ARG+ s" lib/map.f" GS-ARG+ s" lib/map-test.f" GS-ARG+
   s" map stdlib" GS-HB-RUN
   GS-HB s" lib/errors.f" GS-ARG+ s" lib/string.f" GS-ARG+ s" lib/test.f" GS-ARG+ s" lib/fs.f" GS-ARG+ s" lib/fs-mutate.f" GS-ARG+ s" lib/fs-test.f" GS-ARG+
   s" fs stdlib" GS-HB-RUN
   GS-HB s" lib/errors.f" GS-ARG+ s" lib/string.f" GS-ARG+ s" lib/test.f" GS-ARG+ s" lib/fs.f" GS-ARG+ s" lib/fs-mutate.f" GS-ARG+ s" lib/source.f" GS-ARG+ s" lib/source-test.f" GS-ARG+
   s" source stdlib" GS-HB-RUN
   GS-HB s" lib/errors.f" GS-ARG+ s" lib/string.f" GS-ARG+ s" lib/test.f" GS-ARG+ s" lib/fs.f" GS-ARG+ s" lib/fs-mutate.f" GS-ARG+ s" lib/source.f" GS-ARG+ s" lib/source-test.f" GS-ARG+ s" --" GS-ARG+ s" stdin" GS-ARG+
   s" DATA" s" source stdlib stdin" GS-HB-RUN-STDIN
   GS-HB s" lib/errors.f" GS-ARG+ s" lib/string.f" GS-ARG+ s" lib/test.f" GS-ARG+ s" lib/fs.f" GS-ARG+ s" lib/fs-mutate.f" GS-ARG+ s" lib/process.f" GS-ARG+ s" lib/process-argv.f" GS-ARG+ s" tools/hb-cli-contracts-test.f" GS-ARG+
   s" hb CLI contracts" GS-HB-RUN
   GS-HB s" lib/errors.f" GS-ARG+ s" lib/string.f" GS-ARG+ s" lib/test.f" GS-ARG+ s" lib/fs.f" GS-ARG+ s" lib/fs-mutate.f" GS-ARG+ s" lib/process.f" GS-ARG+ s" lib/process-argv.f" GS-ARG+ s" lib/process-test.f" GS-ARG+
   s" process stdlib" GS-HB-RUN
   GS-HB s" lib/argv.f" GS-ARG+ s" lib/argv-test.f" GS-ARG+
   s" argv stdlib mocks" GS-HB-RUN
   GS-HB s" lib/argv.f" GS-ARG+ s" lib/argv-test.f" GS-ARG+ s" --" GS-ARG+ s" --json" GS-ARG+ s" -o" GS-ARG+ s" OUT" GS-ARG+ s" --" GS-ARG+ s" file.f" GS-ARG+ s" --literal" GS-ARG+
   s" argv stdlib script args" GS-HB-RUN
   GS-HB s" lib/test.f" GS-ARG+ s" lib/test-test.f" GS-ARG+
   s" test stdlib" GS-HB-RUN
   GS-HB s" lib/errors.f" GS-ARG+ s" lib/test.f" GS-ARG+ s" lib/property.f" GS-ARG+ s" lib/property-test.f" GS-ARG+
   s" property stdlib" GS-HB-RUN
   GS-HB s" lib/errors.f" GS-ARG+ s" lib/string.f" GS-ARG+ s" lib/test.f" GS-ARG+ s" lib/fs.f" GS-ARG+ s" lib/fs-mutate.f" GS-ARG+ s" lib/process.f" GS-ARG+ s" lib/build.f" GS-ARG+ s" lib/build-test.f" GS-ARG+
   s" build stdlib" GS-HB-RUN
   GS-HB s" tools/date.f" GS-ARG+ s" tools/date-test.f" GS-ARG+
   s" date helpers" GS-HB-RUN ;

: GS-CHECK-STDLIB ( -- )
   s" lib/json-write-test.f" s" json writer helper check" GS-CHECK-JSON-WRITE
   GS-SRC-RESET
   s" lib/errors.f" GS-SRC+ s" lib/string.f" GS-SRC+ s" lib/test.f" GS-SRC+ s" lib/fs.f" GS-SRC+ s" lib/fs-mutate.f" GS-SRC+ s" lib/process.f" GS-SRC+ s" lib/process-argv.f" GS-SRC+ s" lib/test-runner.f" GS-SRC+ s" lib/test-runner-test.f" GS-SRC+
   s" test runner helper check" GS-CHECK-RUN
   GS-SRC-RESET
   s" lib/errors.f" GS-SRC+ s" lib/string.f" GS-SRC+ s" lib/test.f" GS-SRC+ s" lib/fs.f" GS-SRC+ s" lib/fs-mutate.f" GS-SRC+ s" lib/fs-test.f" GS-SRC+
   s" fs stdlib check" GS-CHECK-RUN
   GS-SRC-RESET
   s" lib/errors.f" GS-SRC+ s" lib/string.f" GS-SRC+ s" lib/test.f" GS-SRC+ s" lib/fs.f" GS-SRC+ s" lib/fs-mutate.f" GS-SRC+ s" lib/source.f" GS-SRC+ s" lib/source-test.f" GS-SRC+
   s" source stdlib check" GS-CHECK-RUN
   GS-SRC-RESET
   s" lib/errors.f" GS-SRC+ s" lib/string.f" GS-SRC+ s" lib/test.f" GS-SRC+ s" lib/fs.f" GS-SRC+ s" lib/fs-mutate.f" GS-SRC+ s" lib/process.f" GS-SRC+ s" lib/process-argv.f" GS-SRC+ s" tools/hb-cli-contracts-test.f" GS-SRC+
   s" hb CLI contract check" GS-CHECK-RUN
   GS-SRC-RESET
   s" lib/errors.f" GS-SRC+ s" lib/string.f" GS-SRC+ s" lib/test.f" GS-SRC+ s" lib/fs.f" GS-SRC+ s" lib/fs-mutate.f" GS-SRC+ s" lib/process.f" GS-SRC+ s" lib/process-argv.f" GS-SRC+ s" lib/process-test.f" GS-SRC+
   s" process stdlib check" GS-CHECK-RUN
   GS-SRC-RESET
   s" lib/argv.f" GS-SRC+
   s" argv stdlib check" GS-CHECK-RUN
   GS-SRC-RESET
   s" lib/errors.f" GS-SRC+ s" lib/property.f" GS-SRC+
   s" property stdlib check" GS-CHECK-RUN
   GS-SRC-RESET
   s" lib/errors.f" GS-SRC+ s" lib/string.f" GS-SRC+ s" lib/test.f" GS-SRC+ s" lib/fs.f" GS-SRC+ s" lib/fs-mutate.f" GS-SRC+ s" lib/process.f" GS-SRC+ s" lib/build.f" GS-SRC+ s" lib/build-test.f" GS-SRC+
   s" build stdlib check" GS-CHECK-RUN ;

: GS-RUN-BUILD-HELPERS ( -- )
   GS-HB s" lib/errors.f" GS-ARG+ s" lib/string.f" GS-ARG+ s" lib/test.f" GS-ARG+ s" lib/fs.f" GS-ARG+ s" lib/fs-mutate.f" GS-ARG+ s" lib/process.f" GS-ARG+ s" lib/process-argv.f" GS-ARG+ s" lib/process-env.f" GS-ARG+ s" lib/build.f" GS-ARG+ s" tools/build-fixpoint.f" GS-ARG+ s" tools/build-fixpoint-test.f" GS-ARG+
   s" build fixpoint driver" GS-HB-RUN
   GS-HB s" lib/errors.f" GS-ARG+ s" lib/string.f" GS-ARG+ s" lib/test.f" GS-ARG+ s" lib/fs.f" GS-ARG+ s" lib/fs-mutate.f" GS-ARG+ s" lib/process.f" GS-ARG+ s" lib/process-argv.f" GS-ARG+ s" lib/process-env.f" GS-ARG+ s" lib/source.f" GS-ARG+ s" lib/build.f" GS-ARG+ s" tools/build-fixpoint.f" GS-ARG+ s" tools/hb-build-lib.f" GS-ARG+ s" tools/hb-build-test.f" GS-ARG+
   s" native hb-build fixture" GS-HB-RUN
   GS-HB s" lib/errors.f" GS-ARG+ s" lib/string.f" GS-ARG+ s" lib/test.f" GS-ARG+ s" lib/fs.f" GS-ARG+ s" lib/fs-mutate.f" GS-ARG+ s" lib/process.f" GS-ARG+ s" lib/process-argv.f" GS-ARG+ s" lib/codesign.f" GS-ARG+ s" lib/codesign-test.f" GS-ARG+
   s" codesign helpers" GS-HB-RUN
   GS-HB s" lib/errors.f" GS-ARG+ s" lib/string.f" GS-ARG+ s" lib/test.f" GS-ARG+ s" lib/fs.f" GS-ARG+ s" lib/fs-mutate.f" GS-ARG+ s" lib/process.f" GS-ARG+ s" lib/process-argv.f" GS-ARG+ s" tools/repair-schema-doc-test.f" GS-ARG+
   s" repair diagnostic schema doc" GS-HB-RUN
   GS-HB s" lib/errors.f" GS-ARG+ s" lib/string.f" GS-ARG+ s" lib/test.f" GS-ARG+ s" lib/fs.f" GS-ARG+ s" lib/fs-mutate.f" GS-ARG+ s" lib/process.f" GS-ARG+ s" lib/process-argv.f" GS-ARG+ s" tools/repair-packet-test.f" GS-ARG+
   s" repair packet tool" GS-HB-RUN
   GS-HB s" lib/errors.f" GS-ARG+ s" lib/string.f" GS-ARG+ s" lib/test.f" GS-ARG+ s" lib/fs.f" GS-ARG+ s" lib/fs-mutate.f" GS-ARG+ s" lib/process.f" GS-ARG+ s" lib/process-argv.f" GS-ARG+ s" tools/json-only-test.f" GS-ARG+
   s" json-only tool" GS-HB-RUN
   GS-HB s" lib/errors.f" GS-ARG+ s" lib/string.f" GS-ARG+ s" lib/test.f" GS-ARG+ s" lib/fs.f" GS-ARG+ s" lib/fs-mutate.f" GS-ARG+ s" lib/process.f" GS-ARG+ s" lib/process-argv.f" GS-ARG+ s" tools/check-test.f" GS-ARG+
   s" native check runner" GS-HB-RUN ;

: GS-CHECK-BUILD-HELPERS ( -- )
   GS-SRC-RESET
   s" lib/errors.f" GS-SRC+ s" lib/string.f" GS-SRC+ s" lib/test.f" GS-SRC+ s" lib/fs.f" GS-SRC+ s" lib/fs-mutate.f" GS-SRC+ s" lib/process.f" GS-SRC+ s" lib/process-argv.f" GS-SRC+ s" lib/process-env.f" GS-SRC+ s" lib/build.f" GS-SRC+ s" tools/build-fixpoint.f" GS-SRC+ s" tools/build-fixpoint-test.f" GS-SRC+
   s" build fixpoint fixture check" GS-CHECK-RUN
   GS-SRC-RESET
   s" lib/errors.f" GS-SRC+ s" lib/string.f" GS-SRC+ s" lib/test.f" GS-SRC+ s" lib/fs.f" GS-SRC+ s" lib/fs-mutate.f" GS-SRC+ s" lib/process.f" GS-SRC+ s" lib/process-argv.f" GS-SRC+ s" lib/process-env.f" GS-SRC+ s" lib/source.f" GS-SRC+ s" lib/build.f" GS-SRC+ s" tools/build-fixpoint.f" GS-SRC+ s" tools/hb-build-lib.f" GS-SRC+ s" tools/hb-build-test.f" GS-SRC+
   s" native hb-build fixture check" GS-CHECK-RUN
   GS-SRC-RESET
   s" lib/errors.f" GS-SRC+ s" lib/string.f" GS-SRC+ s" lib/test.f" GS-SRC+ s" lib/fs.f" GS-SRC+ s" lib/fs-mutate.f" GS-SRC+ s" lib/process.f" GS-SRC+ s" lib/process-argv.f" GS-SRC+ s" lib/codesign.f" GS-SRC+ s" lib/codesign-test.f" GS-SRC+
   s" codesign helper check" GS-CHECK-RUN
   s" tools/repair-schema-doc-test.f" s" repair diagnostic schema doc check" GS-CHECK-COMMON-PROC
   s" tools/repair-packet-test.f" s" repair packet fixture check" GS-CHECK-COMMON-PROC
   s" tools/json-only-test.f" s" json-only tool check" GS-CHECK-COMMON-PROC
   s" tools/check-test.f" s" native check runner fixture check" GS-CHECK-COMMON-PROC ;

: GS-RUN-SMALL-TOOLS ( -- )
   GS-HB s" lib/errors.f" GS-ARG+ s" lib/string.f" GS-ARG+ s" lib/test.f" GS-ARG+ s" lib/fs.f" GS-ARG+ s" lib/fs-mutate.f" GS-ARG+ s" src/core/sha256.f" GS-ARG+ s" tools/sha256-file-test.f" GS-ARG+
   s" streaming sha256" GS-HB-RUN ;

: GATE-STDLIB-MAIN ( -- )
   GT-RESET
   GS-CHECK-LINTS
   GS-RUN-TOOL-FIXTURES
   GS-CHECK-TOOL-FIXTURES
   GS-RUN-LLM-FIXTURES
   GS-CHECK-LLM-FIXTURES
   GS-RUN-LLM-DRIVERS
   GS-CHECK-EXPANDED-REPORT
   GS-RUN-SMALL-TOOLS
   GS-RUN-STDLIB
   GS-CHECK-STDLIB
   GS-RUN-BUILD-HELPERS
   GS-CHECK-BUILD-HELPERS
   s" PASS: native lint/stdlib gate phase" type cr ;

GATE-STDLIB-MAIN
