\ gate-stdlib.f - checked runner for the default gate lint/stdlib phase.
\
\ Load after lib/errors.f, lib/string.f, lib/fs.f, lib/fs-mutate.f,
\ lib/process.f, lib/process-argv.f, lib/process-env.f, and
\ lib/test-runner.f.

120000 constant GS-TIMEOUT-MS
64 constant GS-USAGE-RC
128 constant GS-NAME-CAP
1024 constant GS-STDIN-CAP

variable GS-DONE
create GS-LABEL-BUF GS-NAME-CAP allot
create GS-STDIN-BUF GS-STDIN-CAP allot
variable GS-LABEL-U
variable GS-STDIN-U

: GS-USAGE ( -- )
   s" usage: test/gate-stdlib.f" GS-USAGE-RC die ;

: GS-CHECK-ARGS ( -- )
   SCRIPT-ARGC 0= if exit then
   GS-USAGE ;

: GS-CAPTURE-STORE ( -- )
   PROC-OUT-LEN @ LEN>N GT-OUT-U !
   PROC-ERR-LEN @ LEN>N GT-ERR-U !
   PROC-OUTCOME-KIND @ GT-OUTCOME-KIND !
   PROC-OUTCOME-CODE @ GT-OUTCOME-CODE ! ;

: GS-RUN-CAPTURE-LOOP ( ptr u8 n -- ) {: label:ptr labelu :}
   begin PROC-CAPTURE-DONE? 0= while
      GT-PROGRESS-SLICE-MS PROC-POLL-CAPTURE-OUTCOME dup COUNT>N 0= if
         drop
         PROC-REMAINING-MS MS>N 0 <= if PROC-REAP-CAPTURE-TIMEOUT exit then
         label labelu GT-PROGRESS-WAIT
      else
         drop
         GT-OUT-BUF GT-OUT-CAP >LEN GT-ERR-BUF GT-ERR-CAP >LEN PROC-DRAIN-READY
         label labelu GT-PROGRESS-WAIT
      then
   repeat
   PROC-REAP-CAPTURE ;

: GS-POLL-STDIN-CAPTURE ( ms -- count ) {: ms :}
   PROC-OUT-R @ POLLIN 0 >IDX PROC-ARGV-PFD-AT!
   PROC-ERR-R @ POLLIN 1 >IDX PROC-ARGV-PFD-AT!
   PROC-ARGV-IN-W @ FD>N 0 >= if
      PROC-ARGV-IN-W @ POLLOUT 2 >IDX PROC-ARGV-PFD-AT!
   else
      -1 >FD 0 2 >IDX PROC-ARGV-PFD-AT!
   then
   PROC-ARGV-PFD 3 ms MS>N poll {: rc :}
   rc 0 < if E-PROC-OUTPUT PROC-ARGV-THROW-CAPTURE then
   rc >COUNT ;

: GS-RUN-STDIN-CAPTURE-LOOP ( ptr u8 len ptr u8 n -- ) {: in:ptr inu label:ptr labelu :}
   inu LEN>N 0 <= if PROC-ARGV-IN-W PROC-CLOSE-CELL then
   begin PROC-ARGV-STDIN-CAPTURE-DONE? 0= while
      GT-PROGRESS-SLICE-MS GS-POLL-STDIN-CAPTURE dup COUNT>N 0= if
         drop
         PROC-REMAINING-MS MS>N 0 <= if PROC-REAP-CAPTURE-TIMEOUT exit then
         label labelu GT-PROGRESS-WAIT
      else
         drop
         in inu PROC-ARGV-DRIVE-STDIN
         GT-OUT-BUF GT-OUT-CAP >LEN GT-ERR-BUF GT-ERR-CAP >LEN PROC-ARGV-DRAIN-READY
         label labelu GT-PROGRESS-WAIT
      then
   repeat
   PROC-REAP-CAPTURE ;

: GS-RUN-ENV ( ptr u8 n n ptr u8 n -- ) {: path:ptr pathu timeout label:ptr labelu :}
   PROC-ENV-INHERIT-MISSING
   path pathu >LEN PROC-ARGV-CHECK-PATH
   PROC-CAPTURE-RESET
   timeout >MS PROC-CAPTURE-DEADLINE!
   PROC-SETUP-CAPTURE-FDS
   path pathu >LEN PROC-ARGV-PREPARE PROC-ENV-PREPARE PROC-SPAWN-ARGV-ENV-CAPTURE
   label labelu GS-RUN-CAPTURE-LOOP
   PROC-CLOSE-CAPTURE-FDS
   GS-CAPTURE-STORE ;

: GS-RUN-STDIN ( ptr u8 n ptr u8 n n ptr u8 n -- ) {: path:ptr pathu in:ptr inu timeout label:ptr labelu :}
   PROC-ENV-INHERIT-MISSING
   path pathu >LEN PROC-ARGV-CHECK-PATH
   inu 0 < if E-PROC-OUTPUT throw then
   PROC-ARGV-CAPTURE-RESET
   timeout >MS PROC-CAPTURE-DEADLINE!
   PROC-SETUP-CAPTURE-FDS
   PROC-ARGV-SETUP-STDIN-FDS
   path pathu >LEN PROC-ARGV-PREPARE PROC-ENV-PREPARE PROC-SPAWN-ARGV-ENV-STDIN-CAPTURE
   in inu >LEN label labelu GS-RUN-STDIN-CAPTURE-LOOP
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
    >LEN PROC-ARGV+ ;

: GS-CHECK-CAP ( n n -- ) {: u cap :}
   u 0 < if E-STR-BOUNDS throw then
   u cap > if E-STR-CAPACITY throw then ;

: GS-PARSE-NAME ( -- ptr u8 n )
   parse-name dup 0= if 2drop E-STR-BOUNDS throw then ;

: GS-LABEL! ( ptr u8 n -- ) {: src:ptr u :}
   u GS-NAME-CAP GS-CHECK-CAP
   src GS-LABEL-BUF u BYTE-COPY
   u GS-LABEL-U ! ;

: GS-LABEL$ ( -- ptr u8 n )
   GS-LABEL-BUF GS-LABEL-U @ ;

: GS-STDIN! ( ptr u8 n -- ) {: src:ptr u :}
   u GS-STDIN-CAP GS-CHECK-CAP
   src GS-STDIN-BUF u BYTE-COPY
   u GS-STDIN-U ! ;

: GS-STDIN$ ( -- ptr u8 n )
   GS-STDIN-BUF GS-STDIN-U @ ;

: GS-PARSE-LABEL ( -- )
   GS-PARSE-NAME GS-LABEL! ;

: GS-PARSE-STDIN ( -- )
   GS-PARSE-NAME GS-STDIN! ;

: GS-END? ( ptr u8 n -- bool )
   s" ;TEST-SUITE" STR= ;

: GS-PARSE-ARGS ( -- )
   0 GS-DONE !
   begin GS-DONE @ 0= while
      parse-name dup 0= if 2drop E-FS-CAPACITY throw then
      2dup GS-END? if
         2drop -1 GS-DONE !
      else
         GS-ARG+
      then
   repeat ;

: GS-TARGET-UNKNOWN ( -- )
   s" gate-stdlib: unknown target" GS-USAGE-RC die ;

: GS-ARG-TARGET-LAYOUT ( -- )
   HB-TARGET-LINUX? if
      s" src/os/linux/layout.f" GS-ARG+
      exit
   then
   HB-TARGET-MACOS? if
      s" src/os/macos/layout.f" GS-ARG+
      exit
   then
   GS-TARGET-UNKNOWN ;

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

: TEST-SUITE ( -- )
   GS-PARSE-LABEL
   GS-HB
   GS-PARSE-ARGS
   GS-LABEL$ GS-HB-RUN ;

: TEST-SUITE-STDIN ( -- )
   GS-PARSE-LABEL
   GS-PARSE-STDIN
   GS-HB
   GS-PARSE-ARGS
   GS-STDIN$ GS-LABEL$ GS-HB-RUN-STDIN ;

: TEST-SUITE-IMGDUMP ( -- )
   GS-PARSE-LABEL
   GS-HB
   GS-ARG-TARGET-LAYOUT
   GS-PARSE-ARGS
   GS-LABEL$ GS-HB-RUN ;

: GATE-STDLIB-MAIN ( -- )
   GS-CHECK-ARGS
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
   tools/lint/token.f tools/lint/lib.f tools/argv.f tools/trust-lint.f
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

TEST-SUITE tool-boundary-fixtures
   tools/date.f lib/errors.f lib/string.f lib/test.f lib/memory.f
   lib/vector.f lib/fs.f lib/fs-mutate.f lib/process.f
   lib/process-argv.f lib/process-env.f tools/lint/text.f
   tools/lint/intern.f tools/lint/token.f tools/lint/lib.f
   tools/lint/json-writer.f tools/lint/source-lex.f tools/argv.f
   tools/aot-call-report.f tools/repl-lint-test.f
   tools/diag-origin-test.f tools/check-all-errors-test.f
   tools/aot-lint-test.f tools/signature-lint-test.f
   tools/public-signatures-test.f tools/trust-lint-test.f
   tools/stale-status-lint-test.f tools/checked-boundary-lint-test.f
   tools/bundle-lib-test.f tools/examples-test.f
   tools/repair-schema-doc-test.f tools/repair-packet-test.f
   tools/json-only-test.f tools/aot-call-report-test.f
;TEST-SUITE

TEST-SUITE check-cli-boundary
   lib/errors.f lib/string.f lib/test.f lib/fs.f lib/fs-mutate.f
   lib/process.f lib/process-argv.f lib/source.f tools/argv.f
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
   tools/hb-build-lib.f tools/build-fixpoint-test.f tools/hb-build-test.f
   lib/codesign-test.f
;TEST-SUITE

s" PASS: native lint/stdlib gate phase" type cr
