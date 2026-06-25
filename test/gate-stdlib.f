\ gate-stdlib.f - checked runner for the default gate lint/stdlib phase.
\
\ Load after lib/errors.f, lib/string.f, lib/fs.f, lib/fs-mutate.f,
\ lib/process.f, lib/process-argv.f, lib/process-env.f, and
\ lib/test-runner.f.

120000 constant SUITE-TIMEOUT-MS
64 constant SUITE-USAGE-RC
128 constant SUITE-NAME-CAP
1024 constant SUITE-STDIN-CAP

variable SUITE-DONE
create SUITE-LABEL-BUF SUITE-NAME-CAP allot
create SUITE-STDIN-BUF SUITE-STDIN-CAP allot
variable SUITE-LABEL-U
variable SUITE-STDIN-U

: SUITE-USAGE ( -- )
   s" usage: test/gate-stdlib.f" SUITE-USAGE-RC die ;

: SUITE-CHECK-ARGS ( -- )
   SCRIPT-ARGC 0= if exit then
   SUITE-USAGE ;

: SUITE-RUN-ENV ( ptr u8 n n ptr u8 n -- ) {: path:ptr pathu timeout label:ptr labelu :}
   PROC-ENV-INHERIT-MISSING
   path pathu >LEN PROC-ARGV-CHECK-PATH
   PROC-CAPTURE-RESET
   timeout >MS PROC-CAPTURE-DEADLINE!
   PROC-SETUP-CAPTURE-FDS
   path pathu >LEN PROC-ARGV-PREPARE PROC-ENV-PREPARE PROC-SPAWN-ARGV-ENV-CAPTURE
   label labelu GT-PROGRESS-CAPTURE
   PROC-CLOSE-CAPTURE-FDS ;

: SUITE-RUN-STDIN ( ptr u8 n ptr u8 n n ptr u8 n -- ) {: path:ptr pathu in:ptr inu timeout label:ptr labelu :}
   PROC-ENV-INHERIT-MISSING
   path pathu >LEN PROC-ARGV-CHECK-PATH
   inu 0 < if E-PROC-OUTPUT throw then
   PROC-CAPTURE-RESET
   timeout >MS PROC-CAPTURE-DEADLINE!
   PROC-SETUP-CAPTURE-FDS
   PROC-SETUP-STDIN-FDS
   path pathu >LEN PROC-ARGV-PREPARE PROC-ENV-PREPARE PROC-SPAWN-ARGV-ENV-STDIN-CAPTURE
   in inu >LEN label labelu GT-PROGRESS-STDIN-CAPTURE
   PROC-CLOSE-STDIN-FDS
   PROC-CLOSE-CAPTURE-FDS ;

: SUITE-FAIL ( ptr u8 n -- ) {: label:ptr labelu :}
   s" FAIL: " type label labelu type cr
   s" rc: " type GT-RC@ . cr
   GT-OUT$ type
   GT-ERR$ type
   s" gate stdlib phase failed" 1 die ;

: SUITE-EXPECT-OK ( ptr u8 n -- ) {: label:ptr labelu :}
   GT-RC@ 0 <> if label labelu SUITE-FAIL then ;

: SUITE-ARG+ ( ptr u8 n -- )
    >LEN PROC-ARGV+ ;

: SUITE-CHECK-CAP ( n n -- ) {: u cap :}
   u 0 < if E-STR-BOUNDS throw then
   u cap > if E-STR-CAPACITY throw then ;

: SUITE-PARSE-NAME ( -- ptr u8 n )
   parse-name dup 0= if 2drop E-STR-BOUNDS throw then ;

: SUITE-LABEL! ( ptr u8 n -- ) {: src:ptr u :}
   u SUITE-NAME-CAP SUITE-CHECK-CAP
   src SUITE-LABEL-BUF u BYTE-COPY
   u SUITE-LABEL-U ! ;

: SUITE-LABEL$ ( -- ptr u8 n )
   SUITE-LABEL-BUF SUITE-LABEL-U @ ;

: SUITE-STDIN! ( ptr u8 n -- ) {: src:ptr u :}
   u SUITE-STDIN-CAP SUITE-CHECK-CAP
   src SUITE-STDIN-BUF u BYTE-COPY
   u SUITE-STDIN-U ! ;

: SUITE-STDIN$ ( -- ptr u8 n )
   SUITE-STDIN-BUF SUITE-STDIN-U @ ;

: SUITE-PARSE-LABEL ( -- )
   SUITE-PARSE-NAME SUITE-LABEL! ;

: SUITE-PARSE-STDIN ( -- )
   SUITE-PARSE-NAME SUITE-STDIN! ;

: SUITE-END? ( ptr u8 n -- bool )
   s" ;TEST-SUITE" STR= ;

: SUITE-PARSE-ARGS ( -- )
   0 SUITE-DONE !
   begin SUITE-DONE @ 0= while
      parse-name dup 0= if 2drop E-FS-CAPACITY throw then
      2dup SUITE-END? if
         2drop -1 SUITE-DONE !
      else
         SUITE-ARG+
      then
   repeat ;

: SUITE-TARGET-UNKNOWN ( -- )
   s" gate-stdlib: unknown target" SUITE-USAGE-RC die ;

: SUITE-ARG-TARGET-LAYOUT ( -- )
   HB-TARGET-LINUX? if
      s" src/os/linux/layout.f" SUITE-ARG+
      exit
   then
   HB-TARGET-MACOS? if
      s" src/os/macos/layout.f" SUITE-ARG+
      exit
   then
   SUITE-TARGET-UNKNOWN ;

: SUITE-HB ( -- )
   PROC-ARGV-RESET
   s" --load" SUITE-ARG+ ;

: SUITE-HB-RUN ( ptr u8 n -- ) {: label:ptr labelu :}
   label labelu GT-PROGRESS-RUN
   s" bin/hb" SUITE-TIMEOUT-MS label labelu SUITE-RUN-ENV
   label labelu SUITE-EXPECT-OK
   label labelu GT-PROGRESS-PASS ;

: SUITE-HB-RUN-STDIN ( ptr u8 n ptr u8 n -- ) {: in:ptr inu label:ptr labelu :}
   label labelu GT-PROGRESS-RUN
   s" bin/hb" in inu SUITE-TIMEOUT-MS label labelu SUITE-RUN-STDIN
   label labelu SUITE-EXPECT-OK
   label labelu GT-PROGRESS-PASS ;

: TEST-SUITE ( -- )
   SUITE-PARSE-LABEL
   SUITE-HB
   SUITE-PARSE-ARGS
   SUITE-LABEL$ SUITE-HB-RUN ;

: TEST-SUITE-STDIN ( -- )
   SUITE-PARSE-LABEL
   SUITE-PARSE-STDIN
   SUITE-HB
   SUITE-PARSE-ARGS
   SUITE-STDIN$ SUITE-LABEL$ SUITE-HB-RUN-STDIN ;

: TEST-SUITE-IMGDUMP ( -- )
   SUITE-PARSE-LABEL
   SUITE-HB
   SUITE-ARG-TARGET-LAYOUT
   SUITE-PARSE-ARGS
   SUITE-LABEL$ SUITE-HB-RUN ;

: GATE-STDLIB-MAIN ( -- )
   SUITE-CHECK-ARGS
   GT-RESET ;

GATE-STDLIB-MAIN

TEST-SUITE shadow-lint
   tools/lint/text.f tools/lint/token.f tools/lint/lib.f
   tools/lint/shadow-lint.f
;TEST-SUITE

TEST-SUITE clobber-lint
   tools/lint/text.f tools/lint/token.f tools/lint/lib.f
   tools/lint/clobber-lint.f
;TEST-SUITE

TEST-SUITE clobber-lint-fixtures
   tools/lint/text.f tools/lint/token.f tools/lint/lib.f
   tools/lint/clobber-lint.f tools/lint/clobber-lint-test.f
;TEST-SUITE

TEST-SUITE repl-lint
   lib/errors.f lib/memory.f lib/vector.f tools/lint/text.f
   tools/lint/intern.f tools/lint/token.f tools/lint/lib.f tools/argv.f
   tools/repl-lint.f
;TEST-SUITE

TEST-SUITE trust-lint
   tools/date.f lib/errors.f lib/string.f lib/fs.f tools/lint/text.f
   tools/lint/token.f tools/lint/lib.f tools/trust-lint-core.f tools/argv.f
   tools/trust-lint.f
;TEST-SUITE

TEST-SUITE stale-status-lint
   tools/date.f lib/errors.f lib/string.f lib/fs.f tools/lint/text.f
   tools/lint/token.f tools/lint/lib.f tools/argv.f
   tools/stale-status-lint.f
;TEST-SUITE

TEST-SUITE host-lint
   lib/errors.f lib/string.f lib/fs.f tools/lint/text.f
   tools/lint/token.f tools/lint/lib.f tools/host-lint.f
;TEST-SUITE

TEST-SUITE parallel-agent-lint
   tools/lint/text.f tools/lint/token.f tools/lint/lib.f
   tools/parallel-agent-lint.f
;TEST-SUITE

TEST-SUITE filemap-lint
   lib/errors.f lib/memory.f lib/vector.f tools/lint/text.f
   tools/lint/intern.f tools/lint/token.f tools/lint/lib.f
   tools/filemap-lint.f
;TEST-SUITE

TEST-SUITE text-foundation-fixtures
   lib/errors.f lib/memory.f lib/vector.f tools/lint/text.f
   tools/lint/token.f tools/lint/lib.f tools/lint/source-lex.f
   tools/lint/text-foundation-test.f
;TEST-SUITE

TEST-SUITE stdlib-manifest
   lib/errors.f lib/string.f lib/fs.f lib/process.f lib/process-argv.f
   tools/lint/text.f tools/lint/token.f tools/lint/lib.f
   tools/stdlib-manifest-test.f
;TEST-SUITE

TEST-SUITE host-lint-fixtures
   lib/errors.f lib/string.f lib/fs.f tools/lint/text.f
   tools/lint/token.f tools/lint/lib.f tools/host-lint.f
   tools/host-lint-test.f
;TEST-SUITE

TEST-SUITE json-file-cursor
   lib/errors.f lib/string.f lib/test.f lib/memory.f lib/fs.f
   lib/fs-mutate.f tools/json.f tools/json-file.f tools/json-file-test.f
;TEST-SUITE

TEST-SUITE-IMGDUMP imgdump-compare
   src/habu/layout.f lib/errors.f lib/string.f lib/test.f lib/fs.f
   lib/fs-mutate.f lib/process.f lib/process-argv.f tools/imgdump.f
   tools/imgdump-test.f
;TEST-SUITE

TEST-SUITE imagedisasm-tool
   lib/errors.f lib/string.f lib/test.f lib/fs.f lib/fs-mutate.f
   lib/process.f lib/process-argv.f src/arch/arm64/disasm.f
   tools/imagedisasm.f tools/imagedisasm-test.f
;TEST-SUITE

TEST-SUITE tool-boundary-fixtures
   tools/date.f lib/errors.f lib/string.f lib/test.f lib/memory.f
   lib/vector.f lib/fs.f lib/fs-mutate.f lib/process.f
   lib/process-argv.f lib/process-env.f tools/lint/text.f
   tools/lint/intern.f tools/lint/token.f tools/lint/lib.f
   tools/lint/json-writer.f tools/lint/source-lex.f tools/json.f
   tools/diag-origin-core.f tools/json-only-core.f tools/argv.f
   tools/aot-call-report.f
   tools/repl-lint-test.f tools/diag-origin-test.f tools/check-all-errors-test.f
   tools/aot-lint-test.f tools/signature-lint-test.f
   tools/public-signatures-test.f tools/trust-lint-test.f
   tools/stale-status-lint-test.f tools/checked-boundary-lint-test.f
   tools/bundle-lib-test.f tools/examples-test.f
   tools/repair-schema-doc-test.f tools/repair-packet-test.f
   tools/json-only-test.f tools/aot-call-report-test.f
;TEST-SUITE

TEST-SUITE check-cli-boundary
   tools/date.f lib/errors.f lib/string.f lib/test.f lib/memory.f lib/vector.f lib/fs.f lib/fs-mutate.f
   lib/process.f lib/process-argv.f lib/source.f tools/lint/text.f
   tools/lint/token.f tools/lint/lib.f tools/lint/json-writer.f
   tools/lint/source-lex.f tools/diag-origin-core.f tools/json.f
   tools/json-only-core.f tools/signature-lint-core.f
   tools/checked-boundary-lint-core.f tools/trust-lint-core.f
   tools/check-all-errors-core.f tools/argv.f
   tools/check-test.f
;TEST-SUITE

TEST-SUITE streaming-sha256
   lib/errors.f lib/string.f lib/test.f lib/fs.f lib/fs-mutate.f
   src/core/sha256.f tools/sha256-file-test.f
;TEST-SUITE

TEST-SUITE string-helpers
   lib/errors.f lib/string.f lib/string-test.f
;TEST-SUITE

TEST-SUITE array-helpers
   lib/errors.f lib/array.f lib/array-test.f
;TEST-SUITE

TEST-SUITE table-stdlib
   lib/errors.f lib/test.f lib/array.f lib/table.f lib/table-test.f
;TEST-SUITE

TEST-SUITE regex-stdlib
   lib/errors.f lib/string.f lib/test.f lib/regex.f lib/regex-test.f
;TEST-SUITE

TEST-SUITE map-stdlib
   lib/errors.f lib/string.f lib/map.f lib/map-test.f
;TEST-SUITE

TEST-SUITE-STDIN source-stdlib-stdin DATA
   lib/errors.f lib/string.f lib/test.f lib/fs.f lib/fs-mutate.f
   lib/source.f lib/source-test.f -- stdin
;TEST-SUITE

TEST-SUITE argv-stdlib-mocks
   lib/errors.f lib/string.f lib/argv.f lib/argv-test.f
;TEST-SUITE

TEST-SUITE argv-stdlib-script-args
   lib/errors.f lib/string.f lib/argv.f lib/argv-test.f -- --json --label NAME
   --strict-signatures --all-errors --strict-boundary -o OUT -- file.f
   --literal
;TEST-SUITE

TEST-SUITE test-stdlib
   lib/test.f lib/test-test.f
;TEST-SUITE

TEST-SUITE property-stdlib
   lib/errors.f lib/test.f lib/property.f lib/property-test.f
;TEST-SUITE

TEST-SUITE date-helpers
   tools/date.f tools/date-test.f
;TEST-SUITE

TEST-SUITE spawn-emitter-shape
   lib/errors.f lib/string.f lib/test.f lib/fs.f tools/spawn-emitter-test.f
;TEST-SUITE

TEST-SUITE c-call-emitter-shape
   lib/errors.f lib/string.f lib/test.f lib/fs.f tools/c-call-emitter-test.f
;TEST-SUITE

TEST-SUITE signature-scan-emitter-shape
   lib/errors.f lib/string.f lib/test.f lib/fs.f
   tools/signature-scan-emitter-test.f
;TEST-SUITE

TEST-SUITE compiler-dispatch-shape
   lib/errors.f lib/string.f lib/test.f lib/fs.f
   tools/compiler-dispatch-test.f
;TEST-SUITE

TEST-SUITE stdlib-batch-fixtures
   lib/errors.f lib/string.f lib/test.f lib/memory.f lib/vector.f
   lib/json-write.f lib/fs.f lib/fs-mutate.f lib/process.f
   lib/process-argv.f lib/process-env.f lib/test-runner.f lib/source.f
   lib/process-command.f lib/build.f lib/json-write-test.f
   lib/test-runner-test.f lib/memory-test.f lib/vector-test.f lib/fs-test.f
   lib/source-test.f tools/hb-cli-contracts-test.f lib/process-test.f
   lib/process-command-test.f lib/build-test.f
;TEST-SUITE

TEST-SUITE build-helper-fixtures
   lib/errors.f lib/string.f lib/test.f lib/fs.f lib/fs-mutate.f
   lib/process.f lib/process-argv.f lib/process-env.f lib/source.f
   lib/build.f lib/codesign.f tools/build-fixpoint.f
   tools/hb-build-lib.f tools/bootstrap-codegen-test.f
   bootstrap/cg/asm-checked.fs tools/asm-checked-test.f
   src/os/image-bytes.f tools/image-bytes-test.f
   tools/build-fixpoint-test.f tools/hb-build-test.f lib/codesign-test.f
;TEST-SUITE

s" PASS: native lint/stdlib gate phase" type cr
