\ gate-stdlib.f - checked runner for the default gate lint/stdlib phase.
\
\ Load after lib/errors.f, lib/string.f, lib/fs.f, lib/fs-mutate.f,
\ lib/process.f, lib/process-argv.f, lib/process-env.f, and
\ lib/test-runner.f.

128 constant GS-SRC-MAX
120000 constant GS-TIMEOUT-MS

create GS-SRC-A GS-SRC-MAX cells allot
create GS-SRC-LEN GS-SRC-MAX cells allot

variable GS-SRC-N

: GS-CAPTURE-STORE ( -- )
   PROC-OUT-LEN @ GT-OUT-U !
   PROC-ERR-LEN @ GT-ERR-U !
   PROC-OUTCOME-KIND @ GT-OUTCOME-KIND !
   PROC-OUTCOME-CODE @ GT-OUTCOME-CODE ! ;

: GS-RUN-CAPTURE-LOOP ( ptr u8 n -- ) {: label:ptr labelu :}
   begin PROC-CAPTURE-DONE? 0= while
      GT-PROGRESS-SLICE-MS PROC-POLL-CAPTURE-OUTCOME dup 0= if
         drop
         PROC-REMAINING-MS 0 <= if PROC-REAP-CAPTURE-TIMEOUT exit then
         label labelu GT-PROGRESS-WAIT
      else
         drop
         GT-OUT-BUF GT-OUT-CAP GT-ERR-BUF GT-ERR-CAP PROC-DRAIN-READY
         label labelu GT-PROGRESS-WAIT
      then
   repeat
   PROC-REAP-CAPTURE ;

: GS-POLL-STDIN-CAPTURE ( n -- n ) {: ms :}
   PROC-OUT-R @ POLLIN 0 PROC-ARGV-PFD-AT!
   PROC-ERR-R @ POLLIN 1 PROC-ARGV-PFD-AT!
   PROC-ARGV-IN-W @ 0 >= if
      PROC-ARGV-IN-W @ POLLOUT 2 PROC-ARGV-PFD-AT!
   else
      -1 0 2 PROC-ARGV-PFD-AT!
   then
   PROC-ARGV-PFD 3 ms poll {: rc :}
   rc 0 < if E-PROC-OUTPUT PROC-ARGV-THROW-CAPTURE then
   rc ;

: GS-RUN-STDIN-CAPTURE-LOOP ( ptr u8 n ptr u8 n -- ) {: in:ptr inu label:ptr labelu :}
   inu 0 <= if PROC-ARGV-IN-W PROC-CLOSE-CELL then
   begin PROC-ARGV-STDIN-CAPTURE-DONE? 0= while
      GT-PROGRESS-SLICE-MS GS-POLL-STDIN-CAPTURE dup 0= if
         drop
         PROC-REMAINING-MS 0 <= if PROC-REAP-CAPTURE-TIMEOUT exit then
         label labelu GT-PROGRESS-WAIT
      else
         drop
         in inu PROC-ARGV-DRIVE-STDIN
         GT-OUT-BUF GT-OUT-CAP GT-ERR-BUF GT-ERR-CAP PROC-ARGV-DRAIN-READY
         label labelu GT-PROGRESS-WAIT
      then
   repeat
   PROC-REAP-CAPTURE ;

: GS-RUN-ENV ( ptr u8 n n ptr u8 n -- ) {: path:ptr pathu timeout label:ptr labelu :}
   PROC-ENV-INHERIT-MISSING
   path pathu PROC-ARGV-CHECK-PATH
   PROC-CAPTURE-RESET
   timeout PROC-CAPTURE-DEADLINE!
   PROC-SETUP-CAPTURE-FDS
   path pathu PROC-ARGV-PREPARE PROC-ENV-PREPARE PROC-SPAWN-ARGV-ENV-CAPTURE
   label labelu GS-RUN-CAPTURE-LOOP
   PROC-CLOSE-CAPTURE-FDS
   GS-CAPTURE-STORE ;

: GS-RUN-STDIN ( ptr u8 n ptr u8 n n ptr u8 n -- ) {: path:ptr pathu in:ptr inu timeout label:ptr labelu :}
   PROC-ENV-INHERIT-MISSING
   path pathu PROC-ARGV-CHECK-PATH
   inu 0 < if E-PROC-OUTPUT throw then
   PROC-ARGV-CAPTURE-RESET
   timeout PROC-CAPTURE-DEADLINE!
   PROC-SETUP-CAPTURE-FDS
   PROC-ARGV-SETUP-STDIN-FDS
   path pathu PROC-ARGV-PREPARE PROC-ENV-PREPARE PROC-SPAWN-ARGV-ENV-STDIN-CAPTURE
   in inu label labelu GS-RUN-STDIN-CAPTURE-LOOP
   PROC-ARGV-CLOSE-STDIN-FDS
   PROC-CLOSE-CAPTURE-FDS
   GS-CAPTURE-STORE ;

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
   label labelu GT-PROGRESS-RUN
   s" bin/hb" GS-TIMEOUT-MS label labelu GS-RUN-ENV
   label labelu GS-EXPECT-OK
   label labelu GT-PROGRESS-PASS ;

: GS-HB-RUN-STDIN ( ptr u8 n ptr u8 n -- ) {: in:ptr inu label:ptr labelu :}
   label labelu GT-PROGRESS-RUN
   s" bin/hb" in inu GS-TIMEOUT-MS label labelu GS-RUN-STDIN
   label labelu GS-EXPECT-OK
   label labelu GT-PROGRESS-PASS ;

: GS-SRC-RESET ( -- )
   0 GS-SRC-N ! ;

: GS-SRC+ ( ptr u8 n -- ) {: path:ptr pathu :}
   GS-SRC-N @ GS-SRC-MAX >= if E-FS-CAPACITY throw then
   path GS-SRC-A GS-SRC-N @ cells + !
   pathu GS-SRC-LEN GS-SRC-N @ cells + !
   GS-SRC-N @ 1+ GS-SRC-N ! ;

: GS-SRC$ ( n -- ptr u8 n ) {: idx :}
   idx 0 < if E-FS-CAPACITY throw then
   idx GS-SRC-N @ >= if E-FS-CAPACITY throw then
   idx cells GS-SRC-A + @
   idx cells GS-SRC-LEN + @ ;

: GS-SRC-ARGS ( -- )
   0 begin dup GS-SRC-N @ < while
      dup GS-SRC$ GS-ARG+
      1+
   repeat drop ;

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
   label labelu GT-PROGRESS-RUN
   GS-CHECK-ARGV
   s" --source-list" GS-ARG+
   GS-SRC-ARGS
   s" bin/hb" GS-TIMEOUT-MS label labelu GS-RUN-ENV
   label labelu GS-EXPECT-OK
   label labelu GT-PROGRESS-PASS ;

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
   GS-HB s" lib/errors.f" GS-ARG+ s" lib/memory.f" GS-ARG+ s" tools/lint/lib.f" GS-ARG+ s" tools/lint/source-lex.f" GS-ARG+ s" tools/lint/text-foundation-test.f" GS-ARG+
   s" text foundation fixtures" GS-HB-RUN
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
   GS-HB s" lib/errors.f" GS-ARG+ s" lib/string.f" GS-ARG+ s" lib/test.f" GS-ARG+ s" lib/fs.f" GS-ARG+ s" lib/fs-mutate.f" GS-ARG+ s" lib/process.f" GS-ARG+ s" lib/process-argv.f" GS-ARG+ s" lib/process-env.f" GS-ARG+ s" bench/llm/codex-home.f" GS-ARG+ s" bench/llm/codex-home-test.f" GS-ARG+
   s" llm codex home setup" GS-HB-RUN
   GS-HB s" lib/errors.f" GS-ARG+ s" lib/string.f" GS-ARG+
   s" lib/test.f" GS-ARG+ s" lib/memory.f" GS-ARG+
   s" lib/fs.f" GS-ARG+ s" lib/fs-mutate.f" GS-ARG+
   s" lib/process.f" GS-ARG+ s" lib/process-argv.f" GS-ARG+
   s" lib/process-env.f" GS-ARG+ s" tools/json.f" GS-ARG+
   s" tools/argv.f" GS-ARG+ s" bench/llm/manifest.f" GS-ARG+
   s" bench/llm/model.f" GS-ARG+ s" bench/llm/parse-resp-lib.f" GS-ARG+
   s" bench/llm/codex-home.f" GS-ARG+ s" bench/llm/model-run.f" GS-ARG+
   s" bench/llm/model-run-test.f" GS-ARG+
   s" llm model runner" GS-HB-RUN
   GS-HB s" lib/errors.f" GS-ARG+ s" lib/string.f" GS-ARG+ s" lib/test.f" GS-ARG+ s" lib/fs.f" GS-ARG+ s" bench/llm/manifest.f" GS-ARG+ s" bench/llm/manifest-audit.f" GS-ARG+ s" bench/llm/manifest-audit-test.f" GS-ARG+
   s" llm manifest audit" GS-HB-RUN
   GS-HB s" lib/errors.f" GS-ARG+ s" lib/string.f" GS-ARG+ s" lib/test.f" GS-ARG+ s" bench/llm/manifest.f" GS-ARG+ s" bench/llm/vectors.f" GS-ARG+ s" bench/llm/vectors-test.f" GS-ARG+
   s" llm vector parser" GS-HB-RUN
   GS-HB s" lib/errors.f" GS-ARG+ s" lib/string.f" GS-ARG+ s" lib/test.f" GS-ARG+ s" bench/llm/manifest.f" GS-ARG+ s" bench/llm/vectors.f" GS-ARG+ s" bench/llm/foreign-vectors.f" GS-ARG+ s" bench/llm/foreign-vectors-test.f" GS-ARG+
   s" llm foreign vector emitters" GS-HB-RUN
   GS-HB s" lib/errors.f" GS-ARG+ s" lib/string.f" GS-ARG+ s" lib/test.f" GS-ARG+ s" lib/memory.f" GS-ARG+ s" lib/fs.f" GS-ARG+ s" lib/fs-mutate.f" GS-ARG+ s" bench/llm/manifest.f" GS-ARG+ s" bench/llm/forth-task-lines-lib.f" GS-ARG+ s" bench/llm/forth-task-lines-test.f" GS-ARG+
   s" llm forth task row emitter" GS-HB-RUN
   GS-HB s" lib/errors.f" GS-ARG+ s" lib/string.f" GS-ARG+ s" lib/test.f" GS-ARG+ s" bench/llm/manifest.f" GS-ARG+ s" bench/llm/forth-candidate.f" GS-ARG+ s" bench/llm/forth-candidate-test.f" GS-ARG+
   s" llm Forth candidate scanner" GS-HB-RUN
   GS-HB s" lib/errors.f" GS-ARG+ s" lib/string.f" GS-ARG+ s" lib/test.f" GS-ARG+ s" lib/fs.f" GS-ARG+ s" lib/fs-mutate.f" GS-ARG+ s" bench/llm/manifest.f" GS-ARG+ s" bench/llm/forth-bundle.f" GS-ARG+ s" bench/llm/forth-bundle-test.f" GS-ARG+
   s" llm Forth bundle builder" GS-HB-RUN
   GS-HB s" lib/errors.f" GS-ARG+ s" lib/string.f" GS-ARG+ s" lib/test.f" GS-ARG+ s" lib/json-write.f" GS-ARG+ s" tools/json.f" GS-ARG+ s" bench/llm/manifest.f" GS-ARG+ s" bench/llm/diagnostic-stats.f" GS-ARG+ s" bench/llm/diagnostic-stats-test.f" GS-ARG+
   s" llm diagnostic stats" GS-HB-RUN
   GS-HB s" lib/errors.f" GS-ARG+ s" lib/string.f" GS-ARG+ s" lib/test.f" GS-ARG+ s" lib/fs.f" GS-ARG+ s" lib/fs-mutate.f" GS-ARG+ s" src/core/sha256.f" GS-ARG+ s" bench/llm/artifacts.f" GS-ARG+ s" bench/llm/artifacts-test.f" GS-ARG+
   s" llm artifact hashes" GS-HB-RUN
   GS-HB s" lib/errors.f" GS-ARG+ s" lib/string.f" GS-ARG+ s" lib/test.f" GS-ARG+ s" lib/memory.f" GS-ARG+ s" lib/fs.f" GS-ARG+ s" lib/fs-mutate.f" GS-ARG+ s" bench/llm/manifest.f" GS-ARG+ s" tools/lint/lib.f" GS-ARG+ s" tools/lint/source-lex.f" GS-ARG+ s" bench/llm/attempt-solutions-lib.f" GS-ARG+ s" bench/llm/attempt-solutions-test.f" GS-ARG+
   s" llm attempt solution extractor" GS-HB-RUN
   GS-HB s" lib/errors.f" GS-ARG+ s" lib/string.f" GS-ARG+ s" lib/test.f" GS-ARG+ s" lib/memory.f" GS-ARG+ s" lib/fs.f" GS-ARG+ s" lib/fs-mutate.f" GS-ARG+ s" bench/llm/manifest.f" GS-ARG+ s" tools/lint/lib.f" GS-ARG+ s" tools/lint/source-lex.f" GS-ARG+ s" bench/llm/forth-task-lines-lib.f" GS-ARG+ s" bench/llm/attempt-solutions-lib.f" GS-ARG+ s" bench/llm/large-buffer-bundle-test.f" GS-ARG+
   s" llm large buffer bundle" GS-HB-RUN
   GS-HB s" lib/errors.f" GS-ARG+ s" lib/string.f" GS-ARG+ s" lib/test.f" GS-ARG+ s" lib/json-write.f" GS-ARG+ s" tools/json.f" GS-ARG+ s" lib/fs.f" GS-ARG+ s" lib/fs-mutate.f" GS-ARG+ s" lib/process.f" GS-ARG+ s" lib/process-argv.f" GS-ARG+ s" lib/process-env.f" GS-ARG+ s" bench/llm/manifest.f" GS-ARG+ s" bench/llm/diagnostic-stats.f" GS-ARG+ s" bench/llm/run-attempts-lib.f" GS-ARG+ s" bench/llm/run-attempts-test.f" GS-ARG+
   s" llm attempt runner helpers" GS-HB-RUN
   GS-HB s" lib/errors.f" GS-ARG+ s" lib/string.f" GS-ARG+
   s" lib/test.f" GS-ARG+ s" lib/memory.f" GS-ARG+
   s" lib/json-write.f" GS-ARG+ s" tools/json.f" GS-ARG+
   s" lib/fs.f" GS-ARG+ s" lib/fs-mutate.f" GS-ARG+
   s" lib/process.f" GS-ARG+ s" lib/process-argv.f" GS-ARG+
   s" lib/process-env.f" GS-ARG+ s" lib/time.f" GS-ARG+
   s" lib/date.f" GS-ARG+ s" bench/llm/manifest.f" GS-ARG+
   s" tools/lint/lib.f" GS-ARG+ s" tools/lint/source-lex.f" GS-ARG+
   s" tools/argv.f" GS-ARG+ s" bench/llm/forth-task-lines-lib.f" GS-ARG+
   s" bench/llm/attempt-solutions-lib.f" GS-ARG+
   s" bench/llm/diagnostic-stats.f" GS-ARG+
   s" bench/llm/run-attempts-lib.f" GS-ARG+
   s" bench/llm/run-attempts.f" GS-ARG+
   s" bench/llm/run-attempts-cli-test.f" GS-ARG+
   s" llm attempt runner CLI" GS-HB-RUN
   GS-HB s" lib/errors.f" GS-ARG+ s" lib/string.f" GS-ARG+ s" lib/test.f" GS-ARG+ s" lib/fs.f" GS-ARG+ s" lib/fs-mutate.f" GS-ARG+ s" lib/json-write.f" GS-ARG+ s" src/core/sha256.f" GS-ARG+ s" bench/llm/live-row.f" GS-ARG+ s" bench/llm/live-row-test.f" GS-ARG+
   s" llm live row emitter" GS-HB-RUN
   GS-HB s" lib/errors.f" GS-ARG+ s" lib/string.f" GS-ARG+ s" lib/test.f" GS-ARG+ s" lib/json-write.f" GS-ARG+ s" bench/llm/fixture-text.f" GS-ARG+ s" bench/llm/fixture-text-test.f" GS-ARG+
   s" llm fixture text helpers" GS-HB-RUN
   GS-HB s" lib/errors.f" GS-ARG+ s" lib/string.f" GS-ARG+ s" lib/test.f" GS-ARG+ s" lib/json-write.f" GS-ARG+ s" bench/llm/fixture-text.f" GS-ARG+ s" bench/llm/json-row.f" GS-ARG+ s" bench/llm/json-row-test.f" GS-ARG+
   s" llm json row helpers" GS-HB-RUN
   GS-HB s" lib/errors.f" GS-ARG+ s" lib/string.f" GS-ARG+ s" lib/test.f" GS-ARG+ s" bench/llm/negative-score.f" GS-ARG+ s" bench/llm/negative-score-test.f" GS-ARG+
   s" llm negative scorer" GS-HB-RUN
   GS-HB s" lib/errors.f" GS-ARG+ s" lib/string.f" GS-ARG+ s" lib/test.f" GS-ARG+ s" lib/fs.f" GS-ARG+ s" lib/fs-mutate.f" GS-ARG+ s" lib/process.f" GS-ARG+ s" lib/process-argv.f" GS-ARG+ s" bench/llm/grade.f" GS-ARG+ s" bench/llm/grade-test.f" GS-ARG+
   s" llm native grader" GS-HB-RUN
   GS-HB s" lib/errors.f" GS-ARG+ s" lib/string.f" GS-ARG+ s" lib/test.f" GS-ARG+ s" lib/fs.f" GS-ARG+ s" lib/fs-mutate.f" GS-ARG+ s" lib/process.f" GS-ARG+ s" lib/process-argv.f" GS-ARG+ s" lib/process-env.f" GS-ARG+ s" lib/json-write.f" GS-ARG+ s" bench/llm/fixture-text.f" GS-ARG+ s" bench/llm/manifest.f" GS-ARG+ s" bench/llm/validate-results-test.f" GS-ARG+
   s" llm validator positive fixtures" GS-HB-RUN ;

: GS-SRC-LLM-FORTH-BASE ( -- )
   s" lib/errors.f" GS-SRC+ s" lib/string.f" GS-SRC+
   s" lib/test.f" GS-SRC+ s" lib/memory.f" GS-SRC+
   s" lib/fs.f" GS-SRC+ s" lib/fs-mutate.f" GS-SRC+
   s" lib/process.f" GS-SRC+ s" lib/process-argv.f" GS-SRC+
   s" lib/process-env.f" GS-SRC+ s" tools/argv.f" GS-SRC+
   s" tools/json.f" GS-SRC+ s" bench/llm/manifest.f" GS-SRC+
   s" bench/llm/model.f" GS-SRC+ s" bench/llm/parse-resp-lib.f" GS-SRC+
   s" bench/llm/codex-home.f" GS-SRC+ s" bench/llm/model-run.f" GS-SRC+ ;

: GS-SRC-LLM-FORTH-ROW ( -- )
   s" bench/llm/vectors.f" GS-SRC+ s" lib/json-write.f" GS-SRC+
   s" src/core/sha256.f" GS-SRC+ s" bench/llm/live-row.f" GS-SRC+
   s" bench/llm/drive-stdlib-lib.f" GS-SRC+
   s" bench/llm/driver-test-helpers.f" GS-SRC+ ;

: GS-SRC-LLM-FORTH-SCANNERS ( -- )
   s" tools/lint/lib.f" GS-SRC+ s" tools/lint/source-lex.f" GS-SRC+
   s" bench/llm/forth-task-lines-lib.f" GS-SRC+
   s" bench/llm/attempt-solutions-lib.f" GS-SRC+
   s" bench/llm/forth-candidate.f" GS-SRC+
   s" bench/llm/forth-bundle.f" GS-SRC+
   s" bench/llm/drive-forth-lib.f" GS-SRC+ ;

: GS-CHECK-LLM-FORTH-DRIVER ( -- )
   GS-SRC-RESET
   GS-SRC-LLM-FORTH-BASE
   GS-SRC-LLM-FORTH-ROW
   GS-SRC-LLM-FORTH-SCANNERS
   s" bench/llm/drive-forth-test.f" GS-SRC+
   s" llm Forth live driver check" GS-CHECK-RUN ;

: GS-RUN-LLM-FORTH-DRIVER ( -- )
   GS-HB
   s" lib/errors.f" GS-ARG+ s" lib/string.f" GS-ARG+
   s" lib/test.f" GS-ARG+ s" lib/memory.f" GS-ARG+
   s" lib/fs.f" GS-ARG+ s" lib/fs-mutate.f" GS-ARG+
   s" lib/process.f" GS-ARG+ s" lib/process-argv.f" GS-ARG+
   s" lib/process-env.f" GS-ARG+ s" tools/argv.f" GS-ARG+
   s" tools/json.f" GS-ARG+ s" bench/llm/manifest.f" GS-ARG+
   s" bench/llm/model.f" GS-ARG+ s" bench/llm/parse-resp-lib.f" GS-ARG+
   s" bench/llm/codex-home.f" GS-ARG+ s" bench/llm/model-run.f" GS-ARG+
   s" bench/llm/vectors.f" GS-ARG+ s" lib/json-write.f" GS-ARG+
   s" src/core/sha256.f" GS-ARG+ s" bench/llm/live-row.f" GS-ARG+
   s" bench/llm/drive-stdlib-lib.f" GS-ARG+
   s" bench/llm/driver-test-helpers.f" GS-ARG+
   s" tools/lint/lib.f" GS-ARG+ s" tools/lint/source-lex.f" GS-ARG+
   s" bench/llm/forth-task-lines-lib.f" GS-ARG+
   s" bench/llm/attempt-solutions-lib.f" GS-ARG+
   s" bench/llm/forth-candidate.f" GS-ARG+
   s" bench/llm/forth-bundle.f" GS-ARG+
   s" bench/llm/drive-forth-lib.f" GS-ARG+
   s" bench/llm/drive-forth-test.f" GS-ARG+
   s" llm Forth live driver" GS-HB-RUN ;

: GS-CHECK-LLM-FIXTURES ( -- )
   GS-SRC-RESET
   s" lib/errors.f" GS-SRC+ s" lib/string.f" GS-SRC+ s" lib/test.f" GS-SRC+ s" lib/fs.f" GS-SRC+ s" bench/llm/manifest.f" GS-SRC+ s" bench/llm/model.f" GS-SRC+ s" bench/llm/model-test.f" GS-SRC+
   s" llm model registry check" GS-CHECK-RUN
   GS-SRC-RESET
   s" lib/errors.f" GS-SRC+ s" lib/string.f" GS-SRC+ s" lib/test.f" GS-SRC+ s" lib/fs.f" GS-SRC+ s" lib/fs-mutate.f" GS-SRC+ s" lib/process.f" GS-SRC+ s" lib/process-argv.f" GS-SRC+ s" lib/process-env.f" GS-SRC+ s" bench/llm/codex-home.f" GS-SRC+ s" bench/llm/codex-home-test.f" GS-SRC+
   s" llm codex home setup check" GS-CHECK-RUN
   GS-SRC-RESET
   s" lib/errors.f" GS-SRC+ s" lib/string.f" GS-SRC+ s" lib/test.f" GS-SRC+ s" lib/fs.f" GS-SRC+ s" bench/llm/manifest.f" GS-SRC+ s" bench/llm/manifest-audit.f" GS-SRC+ s" bench/llm/manifest-audit-test.f" GS-SRC+
   s" llm manifest audit check" GS-CHECK-RUN
   GS-SRC-RESET
   s" lib/errors.f" GS-SRC+ s" lib/string.f" GS-SRC+ s" lib/test.f" GS-SRC+ s" bench/llm/manifest.f" GS-SRC+ s" bench/llm/vectors.f" GS-SRC+ s" bench/llm/vectors-test.f" GS-SRC+
   s" llm vector parser check" GS-CHECK-RUN
   GS-SRC-RESET
   s" lib/errors.f" GS-SRC+ s" lib/string.f" GS-SRC+ s" lib/test.f" GS-SRC+ s" bench/llm/manifest.f" GS-SRC+ s" bench/llm/vectors.f" GS-SRC+ s" bench/llm/foreign-vectors.f" GS-SRC+ s" bench/llm/foreign-vectors-test.f" GS-SRC+
   s" llm foreign vector emitters check" GS-CHECK-RUN
   GS-SRC-RESET
   s" lib/errors.f" GS-SRC+ s" lib/string.f" GS-SRC+ s" lib/test.f" GS-SRC+ s" lib/memory.f" GS-SRC+ s" lib/fs.f" GS-SRC+ s" lib/fs-mutate.f" GS-SRC+ s" bench/llm/manifest.f" GS-SRC+ s" bench/llm/forth-task-lines-lib.f" GS-SRC+ s" bench/llm/forth-task-lines-test.f" GS-SRC+
   s" llm forth task row emitter check" GS-CHECK-RUN
   GS-SRC-RESET
   s" lib/errors.f" GS-SRC+ s" lib/string.f" GS-SRC+ s" lib/test.f" GS-SRC+ s" bench/llm/manifest.f" GS-SRC+ s" bench/llm/forth-candidate.f" GS-SRC+ s" bench/llm/forth-candidate-test.f" GS-SRC+
   s" llm Forth candidate scanner check" GS-CHECK-RUN
   GS-SRC-RESET
   s" lib/errors.f" GS-SRC+ s" lib/string.f" GS-SRC+ s" lib/test.f" GS-SRC+ s" lib/fs.f" GS-SRC+ s" lib/fs-mutate.f" GS-SRC+ s" bench/llm/manifest.f" GS-SRC+ s" bench/llm/forth-bundle.f" GS-SRC+ s" bench/llm/forth-bundle-test.f" GS-SRC+
   s" llm Forth bundle builder check" GS-CHECK-RUN
   GS-SRC-RESET
   s" lib/errors.f" GS-SRC+ s" lib/string.f" GS-SRC+ s" lib/test.f" GS-SRC+ s" lib/json-write.f" GS-SRC+ s" bench/llm/manifest.f" GS-SRC+ s" bench/llm/diagnostic-json-check-stub.f" GS-SRC+ s" bench/llm/diagnostic-stats.f" GS-SRC+ s" bench/llm/diagnostic-stats-check-test.f" GS-SRC+
   s" llm diagnostic stats check" GS-CHECK-RUN
   GS-SRC-RESET
   s" lib/errors.f" GS-SRC+ s" lib/string.f" GS-SRC+ s" lib/test.f" GS-SRC+ s" lib/fs.f" GS-SRC+ s" lib/fs-mutate.f" GS-SRC+ s" src/core/sha256.f" GS-SRC+ s" bench/llm/artifacts.f" GS-SRC+ s" bench/llm/artifacts-test.f" GS-SRC+
   s" llm artifact hash check" GS-CHECK-RUN
   GS-SRC-RESET
   s" lib/errors.f" GS-SRC+ s" lib/string.f" GS-SRC+ s" lib/test.f" GS-SRC+ s" lib/json-write.f" GS-SRC+ s" bench/llm/diagnostic-json-check-stub.f" GS-SRC+ s" lib/fs.f" GS-SRC+ s" lib/fs-mutate.f" GS-SRC+ s" lib/process.f" GS-SRC+ s" lib/process-argv.f" GS-SRC+ s" lib/process-env.f" GS-SRC+ s" bench/llm/manifest.f" GS-SRC+ s" bench/llm/diagnostic-stats.f" GS-SRC+ s" bench/llm/run-attempts-lib.f" GS-SRC+ s" bench/llm/run-attempts-check-test.f" GS-SRC+
   s" llm attempt runner helper check" GS-CHECK-RUN
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
   s" llm native grader check" GS-CHECK-RUN
   ;

: GS-ARG-LLM-DRIVER-BASE ( -- )
   GS-HB
   s" lib/errors.f" GS-ARG+ s" lib/string.f" GS-ARG+
   s" lib/test.f" GS-ARG+ s" lib/memory.f" GS-ARG+
   s" lib/fs.f" GS-ARG+ s" lib/fs-mutate.f" GS-ARG+
   s" lib/process.f" GS-ARG+ s" lib/process-argv.f" GS-ARG+
   s" lib/process-env.f" GS-ARG+ s" tools/argv.f" GS-ARG+
   s" tools/json.f" GS-ARG+ s" bench/llm/manifest.f" GS-ARG+
   s" bench/llm/model.f" GS-ARG+ s" bench/llm/parse-resp-lib.f" GS-ARG+
   s" bench/llm/codex-home.f" GS-ARG+ s" bench/llm/model-run.f" GS-ARG+
   s" bench/llm/vectors.f" GS-ARG+ s" lib/json-write.f" GS-ARG+
   s" src/core/sha256.f" GS-ARG+ s" bench/llm/live-row.f" GS-ARG+
   s" bench/llm/drive-stdlib-lib.f" GS-ARG+ ;

: GS-ARG-DRIVER-TEST ( -- )
   s" bench/llm/driver-test-helpers.f" GS-ARG+ ;

: GS-ARG-DRIVER-FIXTURE ( -- )
   s" bench/llm/driver-fixture-helpers.f" GS-ARG+ ;

: GS-ARG-DRIVER-TOKEN ( -- )
   s" bench/llm/driver-token-helpers.f" GS-ARG+ ;

: GS-RUN-LLM-DRIVERS ( -- )
   GS-ARG-LLM-DRIVER-BASE
   GS-ARG-DRIVER-TEST
   s" bench/llm/drive-stdlib-test.f" GS-ARG+
   s" llm stdlib stack driver" GS-HB-RUN
   GS-ARG-LLM-DRIVER-BASE
   GS-ARG-DRIVER-TEST
   s" bench/llm/drive-regex-negative-lib.f" GS-ARG+
   s" bench/llm/drive-regex-negative-test.f" GS-ARG+
   s" llm stdlib regex negative driver" GS-HB-RUN
   GS-ARG-LLM-DRIVER-BASE
   GS-ARG-DRIVER-TEST
   GS-ARG-DRIVER-FIXTURE
   s" bench/llm/drive-file-lib.f" GS-ARG+
   s" bench/llm/drive-file-test.f" GS-ARG+
   s" llm stdlib file driver" GS-HB-RUN
   GS-ARG-LLM-DRIVER-BASE
   GS-ARG-DRIVER-TEST
   GS-ARG-DRIVER-FIXTURE
   s" bench/llm/drive-process-lib.f" GS-ARG+
   s" bench/llm/drive-process-test.f" GS-ARG+
   s" llm stdlib process driver" GS-HB-RUN
   GS-ARG-LLM-DRIVER-BASE
   GS-ARG-DRIVER-TEST
   GS-ARG-DRIVER-TOKEN
   GS-ARG-DRIVER-FIXTURE
   s" bench/llm/drive-property-lib.f" GS-ARG+
   s" bench/llm/drive-property-test.f" GS-ARG+
   s" llm stdlib property driver" GS-HB-RUN
   GS-ARG-LLM-DRIVER-BASE
   GS-ARG-DRIVER-TEST
   GS-ARG-DRIVER-TOKEN
   GS-ARG-DRIVER-FIXTURE
   s" bench/llm/drive-build-lib.f" GS-ARG+
   s" bench/llm/drive-build-test.f" GS-ARG+
   s" llm stdlib build driver" GS-HB-RUN
   GS-ARG-LLM-DRIVER-BASE
   GS-ARG-DRIVER-TEST
   GS-ARG-DRIVER-TOKEN
   s" bench/llm/drive-aot-lib.f" GS-ARG+
   s" bench/llm/drive-aot-test.f" GS-ARG+
   s" llm AOT driver" GS-HB-RUN
   GS-RUN-LLM-FORTH-DRIVER
   GS-ARG-LLM-DRIVER-BASE
   GS-ARG-DRIVER-TOKEN
   s" bench/llm/drive-array-habu-lib.f" GS-ARG+
   s" bench/llm/drive-array-habu-test.f" GS-ARG+
   s" llm Habu array driver" GS-HB-RUN
   GS-HB s" lib/errors.f" GS-ARG+ s" lib/string.f" GS-ARG+ s" lib/test.f" GS-ARG+ s" lib/fs.f" GS-ARG+ s" lib/fs-mutate.f" GS-ARG+ s" lib/process.f" GS-ARG+ s" lib/process-argv.f" GS-ARG+ s" lib/process-env.f" GS-ARG+ s" lib/source.f" GS-ARG+ s" lib/build.f" GS-ARG+ s" tools/build-fixpoint.f" GS-ARG+ s" tools/hb-build-lib.f" GS-ARG+ s" lib/json-write.f" GS-ARG+ s" bench/llm/fixture-text.f" GS-ARG+ s" bench/llm/drive-array-habu-repair-test.f" GS-ARG+
   s" llm Habu array repair driver" GS-HB-RUN
   GS-HB s" lib/errors.f" GS-ARG+ s" lib/string.f" GS-ARG+ s" lib/test.f" GS-ARG+ s" lib/fs.f" GS-ARG+ s" lib/fs-mutate.f" GS-ARG+ s" lib/process.f" GS-ARG+ s" lib/process-argv.f" GS-ARG+ s" lib/process-env.f" GS-ARG+ s" lib/json-write.f" GS-ARG+ s" bench/llm/fixture-text.f" GS-ARG+ s" bench/llm/run-expanded-bench-test.f" GS-ARG+
   s" llm expanded runner" GS-HB-RUN
   GS-HB s" lib/errors.f" GS-ARG+ s" lib/string.f" GS-ARG+ s" lib/test.f" GS-ARG+ s" lib/json-write.f" GS-ARG+ s" lib/fs.f" GS-ARG+ s" lib/fs-mutate.f" GS-ARG+ s" lib/process.f" GS-ARG+ s" lib/process-argv.f" GS-ARG+ s" bench/llm/report-test.f" GS-ARG+
   s" llm report" GS-HB-RUN
   GS-HB s" lib/errors.f" GS-ARG+ s" lib/string.f" GS-ARG+ s" lib/test.f" GS-ARG+ s" lib/json-write.f" GS-ARG+ s" lib/fs.f" GS-ARG+ s" lib/fs-mutate.f" GS-ARG+ s" lib/process.f" GS-ARG+ s" lib/process-argv.f" GS-ARG+ s" bench/llm/expanded-report-test.f" GS-ARG+
   s" llm expanded report" GS-HB-RUN ;

: GS-CHECK-EXPANDED-REPORT ( -- )
   GS-SRC-RESET
   s" lib/errors.f" GS-SRC+ s" lib/string.f" GS-SRC+ s" lib/test.f" GS-SRC+ s" lib/json-write.f" GS-SRC+ s" lib/fs.f" GS-SRC+ s" lib/fs-mutate.f" GS-SRC+ s" lib/process.f" GS-SRC+ s" lib/process-argv.f" GS-SRC+ s" bench/llm/report-test.f" GS-SRC+
   s" llm report check" GS-CHECK-RUN
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
   GS-HB s" lib/errors.f" GS-ARG+ s" lib/test.f" GS-ARG+ s" lib/memory.f" GS-ARG+ s" lib/memory-test.f" GS-ARG+
   s" memory stdlib" GS-HB-RUN
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
   s" lib/errors.f" GS-SRC+ s" lib/test.f" GS-SRC+ s" lib/memory.f" GS-SRC+ s" lib/memory-test.f" GS-SRC+
   s" memory stdlib check" GS-CHECK-RUN
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
