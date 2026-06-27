\ gate-stdlib.f - checked runner for the default gate lint/stdlib phase.
\
\ Load after lib/errors.f, lib/string.f, lib/fs.f, lib/fs-mutate.f,
\ lib/process.f, lib/process-argv.f, lib/process-env.f, lib/test-runner.f,
\ and test/gate-pool.f.

120000 constant SUITE-TIMEOUT-MS
64 constant SUITE-USAGE-RC
128 constant SUITE-NAME-CAP
1024 constant SUITE-STDIN-CAP
0 constant SUITE-ALL-ID
1 constant SUITE-WARM-ID
2 constant SUITE-LINT-ID
3 constant SUITE-TOOL-ID
4 constant SUITE-CHECK-CLI-ID
5 constant SUITE-TAIL-ID

variable SUITE-DONE
create SUITE-LABEL-BUF SUITE-NAME-CAP allot
create SUITE-STDIN-BUF SUITE-STDIN-CAP allot
create SUITE-WARM-BUF FS-PATH-CAP allot
create SUITE-WARM-TRUST-BUF FS-PATH-CAP allot
create SUITE-WARM-OUT GT-OUT-CAP allot
create SUITE-WARM-ERR GT-ERR-CAP allot
variable SUITE-LABEL-U
variable SUITE-STDIN-U
variable SUITE-WARM-U
variable SUITE-WARM-TRUST-U
variable SUITE-OWN-ROOT
variable SUITE-SLICE

: SUITE-TRUE ( -- bool )
   0 0= ;

: SUITE-FALSE ( -- bool )
   0 0= 0= ;

: SUITE-USAGE ( -- )
   s" usage: test/gate-stdlib.f [warm|lint|tool|check-cli|tail]" SUITE-USAGE-RC die ;

: SUITE-ARG0= ( ptr u8 n -- bool )
   0 SCRIPT-ARGV$ STR= ;

: SUITE-SLICE! ( n -- )
   SUITE-SLICE ! ;

: SUITE-PARSE-SLICE ( -- )
   SUITE-ALL-ID SUITE-SLICE!
   SCRIPT-ARGC 0= if exit then
   SCRIPT-ARGC 1 <> if SUITE-USAGE then
   s" warm" SUITE-ARG0= if SUITE-WARM-ID SUITE-SLICE! exit then
   s" lint" SUITE-ARG0= if SUITE-LINT-ID SUITE-SLICE! exit then
   s" tool" SUITE-ARG0= if SUITE-TOOL-ID SUITE-SLICE! exit then
   s" check-cli" SUITE-ARG0= if SUITE-CHECK-CLI-ID SUITE-SLICE! exit then
   s" tail" SUITE-ARG0= if SUITE-TAIL-ID SUITE-SLICE! exit then
   SUITE-USAGE ;

: SUITE-CHECK-ARGS ( -- )
   SUITE-PARSE-SLICE ;

: SUITE-ENV ( -- )
   PROC-ENV-RESET
   s" HABU_WARM_TOOLS" >LEN SUITE-WARM-BUF SUITE-WARM-U @ >LEN PROC-ENV+
   s" HABU_WARM_TOOLS_TRUST" >LEN SUITE-WARM-TRUST-BUF SUITE-WARM-TRUST-U @ >LEN PROC-ENV+
   PROC-ENV-INHERIT-MISSING ;

: SUITE-RUN-ENV ( ptr u8 n n ptr u8 n -- ) {: path:ptr pathu timeout label:ptr labelu :}
   SUITE-ENV
   path pathu >LEN PROC-ARGV-CHECK-PATH
   PROC-CAPTURE-RESET
   timeout >MS PROC-CAPTURE-DEADLINE!
   PROC-SETUP-CAPTURE-FDS
   path pathu >LEN PROC-ARGV-PREPARE PROC-ENV-PREPARE PROC-SPAWN-ARGV-ENV-CAPTURE
   label labelu GT-PROGRESS-CAPTURE
   PROC-CLOSE-CAPTURE-FDS ;

: SUITE-RUN-STDIN ( ptr u8 n ptr u8 n n ptr u8 n -- ) {: path:ptr pathu in:ptr inu timeout label:ptr labelu :}
   SUITE-ENV
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

: SUITE-RUN-ENV-ASYNC ( ptr u8 n n ptr u8 n -- ) {: path:ptr pathu timeout label:ptr labelu :}
   SUITE-ENV
   path pathu label labelu timeout GT-POOL-START ;

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

: SUITE-SUFFIX! ( ptr u8 n ptr u8 n ptr u8 ptr n -- )
   {: a:ptr u suf:ptr su dst:ptr lenp:ptr :}
   u su + FS-PATH-CAP > if E-FS-PATH throw then
   a dst u BYTE-COPY
   suf dst u + su BYTE-COPY
   u su + lenp ! ;

: SUITE-WARM$ ( -- ptr u8 n )
   SUITE-WARM-BUF SUITE-WARM-U @ ;

: SUITE-WARM-TRUST$ ( -- ptr u8 n )
   SUITE-WARM-TRUST-BUF SUITE-WARM-TRUST-U @ ;

: SUITE-SET-ROOT ( -- )
   0 SUITE-OWN-ROOT !
   s" HABU_GATE_WARM_ROOT" GETENV dup 0= if
      2drop
      CLEANUP-RESET
      s" hb-stdlib-warm" TMPDIR-MKDIR GT-COPY-ROOT!
      GT-ROOT CLEANUP-TREE+
      -1 SUITE-OWN-ROOT !
      exit
   then
   GT-COPY-ROOT! ;

: SUITE-WARM-PATHS ( -- )
   GT-ROOT s" hb-tools-warm" SUITE-WARM-BUF JOIN-PATH SUITE-WARM-U !
   SUITE-WARM$ s" .trust.f" SUITE-WARM-TRUST-BUF SUITE-WARM-TRUST-U SUITE-SUFFIX! ;

: SUITE-WARM-CACHED? ( -- bool )
   SUITE-WARM$ EXECUTABLE?
   SUITE-WARM-TRUST$ FILE?
   and ;

: SUITE-WARM-TOOL-ARGV ( -- )
   PROC-ARGV-ENV-RESET
   s" --load" SUITE-ARG+
   s" lib/errors.f" SUITE-ARG+
   s" lib/string.f" SUITE-ARG+
   s" lib/memory.f" SUITE-ARG+
   s" lib/fs.f" SUITE-ARG+
   s" lib/fs-mutate.f" SUITE-ARG+
   s" lib/process.f" SUITE-ARG+
   s" lib/process-argv.f" SUITE-ARG+
   s" lib/process-env.f" SUITE-ARG+
   s" lib/source.f" SUITE-ARG+
   s" lib/codesign.f" SUITE-ARG+
   s" tools/warm-image-lib.f" SUITE-ARG+
   s" tools/warm-image.f" SUITE-ARG+
   s" --" SUITE-ARG+
   SUITE-WARM$ SUITE-ARG+ ;

: SUITE-WARM-SUPPORT-ARGV ( -- )
   s" tools/date.f" SUITE-ARG+
   s" lib/errors.f" SUITE-ARG+
   s" lib/string.f" SUITE-ARG+
   s" lib/memory.f" SUITE-ARG+
   s" lib/vector.f" SUITE-ARG+
   s" lib/fs.f" SUITE-ARG+
   s" lib/fs-mutate.f" SUITE-ARG+
   s" lib/process.f" SUITE-ARG+
   s" lib/process-argv.f" SUITE-ARG+
   s" lib/source.f" SUITE-ARG+
   s" tools/lint/text.f" SUITE-ARG+
   s" tools/lint/intern.f" SUITE-ARG+
   s" tools/lint/token.f" SUITE-ARG+
   s" tools/lint/lib.f" SUITE-ARG+
   s" tools/lint/json-writer.f" SUITE-ARG+
   s" tools/lint/source-lex.f" SUITE-ARG+
   s" tools/diag-origin-core.f" SUITE-ARG+
   s" tools/json.f" SUITE-ARG+
   s" tools/gate-json-assert-core.f" SUITE-ARG+
   s" tools/json-only-core.f" SUITE-ARG+
   s" tools/signature-lint-core.f" SUITE-ARG+
   s" tools/checked-boundary-lint-core.f" SUITE-ARG+
   s" tools/trust-lint-core.f" SUITE-ARG+
   s" tools/check-all-errors-core.f" SUITE-ARG+
   s" tools/argv.f" SUITE-ARG+ ;

: SUITE-WARM-PRINT ( n n -- ) {: outu erru :}
   SUITE-WARM-OUT outu type
   SUITE-WARM-ERR erru type ;

: SUITE-OUTCOME. ( n -- ) {: kind :}
   kind PROC-OUTCOME-EXIT = if s" exit" type exit then
   kind PROC-OUTCOME-SIGNAL = if s" signal" type exit then
   kind PROC-OUTCOME-TIMEOUT = if s" timeout" type exit then
   s" unknown" type ;

: SUITE-WARM-OK? ( n n -- bool ) {: kind code :}
   kind PROC-OUTCOME-EXIT =
   code 0= and ;

: SUITE-WARM-FAIL ( len len n n -- ) {: outu erru kind code :}
   s" FAIL: gate-stdlib warm tools image" type cr
   s" outcome: " type kind SUITE-OUTCOME.
   s"  code: " type code . cr
   s" rc: " type kind code PROC-OUTCOME>RC RC>N . cr
   s" stdout bytes: " type outu LEN>N . s" / " type GT-OUT-CAP . cr
   s" stderr bytes: " type erru LEN>N . s" / " type GT-ERR-CAP . cr
   s" stdout:" type cr
   SUITE-WARM-OUT outu LEN>N type
   s" stderr:" type cr
   SUITE-WARM-ERR erru LEN>N type
   s" gate-stdlib: warm tools image failed" 1 die ;

: SUITE-WARM-RUN ( -- )
   SUITE-WARM-TOOL-ARGV
   SUITE-WARM-SUPPORT-ARGV
   PROC-ENV-RESET
   PROC-ENV-INHERIT-MISSING
   s" bin/hb" >LEN SUITE-WARM-OUT GT-OUT-CAP >LEN SUITE-WARM-ERR GT-ERR-CAP >LEN
   SUITE-TIMEOUT-MS >MS RUN-ARGV-ENV-CAPTURE-OUTCOME
   {: outu erru kind code :}
   kind code SUITE-WARM-OK? 0= if
      outu erru kind code SUITE-WARM-FAIL
   then ;

: SUITE-WARM-PREPARE ( -- )
   SUITE-SET-ROOT
   SUITE-WARM-PATHS
   SUITE-WARM-CACHED? if exit then
   SUITE-WARM-RUN ;

: SUITE-CLEANUP ( -- )
   SUITE-OWN-ROOT @ if CLEANUP-RUN then ;

: SUITE-PARSE-NAME ( -- ptr u8 n )
   parse-name dup 0= if 2drop E-STR-BOUNDS throw then ;

: SUITE-LABEL! ( ptr u8 n -- ) {: src:ptr u :}
   u SUITE-NAME-CAP SUITE-CHECK-CAP
   src SUITE-LABEL-BUF u BYTE-COPY
   u SUITE-LABEL-U ! ;

: SUITE-LABEL$ ( -- ptr u8 n )
   SUITE-LABEL-BUF SUITE-LABEL-U @ ;

: SUITE-LABEL= ( ptr u8 n -- bool ) {: a:ptr u :}
   SUITE-LABEL$ a u STR= ;

: SUITE-ALL? ( -- bool )
   SUITE-SLICE @ SUITE-ALL-ID = ;

: SUITE-WARM? ( -- bool )
   SUITE-SLICE @ SUITE-WARM-ID = ;

: SUITE-LINT? ( -- bool )
   SUITE-SLICE @ SUITE-LINT-ID <> if SUITE-FALSE exit then
   s" shadow-lint" SUITE-LABEL= if SUITE-TRUE exit then
   s" clobber-lint" SUITE-LABEL= if SUITE-TRUE exit then
   s" clobber-lint-fixtures" SUITE-LABEL= if SUITE-TRUE exit then
   s" repl-lint" SUITE-LABEL= if SUITE-TRUE exit then
   s" trust-lint" SUITE-LABEL= if SUITE-TRUE exit then
   s" stale-status-lint" SUITE-LABEL= if SUITE-TRUE exit then
   s" host-lint" SUITE-LABEL= if SUITE-TRUE exit then
   s" parallel-agent-lint" SUITE-LABEL= if SUITE-TRUE exit then
   s" filemap-lint" SUITE-LABEL= if SUITE-TRUE exit then
   s" text-foundation-fixtures" SUITE-LABEL= if SUITE-TRUE exit then
   s" stdlib-manifest" SUITE-LABEL= if SUITE-TRUE exit then
   s" host-lint-fixtures" SUITE-LABEL= if SUITE-TRUE exit then
   s" json-file-cursor" SUITE-LABEL= if SUITE-TRUE exit then
   s" imgdump-compare" SUITE-LABEL= if SUITE-TRUE exit then
   s" imagedisasm-tool" SUITE-LABEL= if SUITE-TRUE exit then
   s" streaming-sha256" SUITE-LABEL= if SUITE-TRUE exit then
   s" string-helpers" SUITE-LABEL= if SUITE-TRUE exit then
   s" array-helpers" SUITE-LABEL= if SUITE-TRUE exit then
   s" table-stdlib" SUITE-LABEL= if SUITE-TRUE exit then
   s" regex-stdlib" SUITE-LABEL= if SUITE-TRUE exit then
   s" map-stdlib" SUITE-LABEL= if SUITE-TRUE exit then
   s" ptx-stdlib" SUITE-LABEL= if SUITE-TRUE exit then
   s" ptx-toolchain" SUITE-LABEL= if SUITE-TRUE exit then
   SUITE-FALSE ;

: SUITE-TOOL? ( -- bool )
   SUITE-SLICE @ SUITE-TOOL-ID <> if SUITE-FALSE exit then
   s" tool-boundary-trust" SUITE-LABEL= if SUITE-TRUE exit then
   s" tool-boundary-check-repair" SUITE-LABEL= if SUITE-TRUE exit then
   s" tool-boundary-doc-public" SUITE-LABEL= if SUITE-TRUE exit then
   s" tool-boundary-lints" SUITE-LABEL= if SUITE-TRUE exit then
   SUITE-FALSE ;

: SUITE-CHECK-CLI? ( -- bool )
   SUITE-SLICE @ SUITE-CHECK-CLI-ID <> if SUITE-FALSE exit then
   s" check-cli-boundary" SUITE-LABEL= ;

: SUITE-TAIL? ( -- bool )
   SUITE-SLICE @ SUITE-TAIL-ID <> if SUITE-FALSE exit then
   s" source-stdlib-stdin" SUITE-LABEL= if SUITE-TRUE exit then
   s" argv-stdlib-mocks" SUITE-LABEL= if SUITE-TRUE exit then
   s" argv-stdlib-script-args" SUITE-LABEL= if SUITE-TRUE exit then
   s" test-stdlib" SUITE-LABEL= if SUITE-TRUE exit then
   s" property-stdlib" SUITE-LABEL= if SUITE-TRUE exit then
   s" date-helpers" SUITE-LABEL= if SUITE-TRUE exit then
   s" spawn-emitter-shape" SUITE-LABEL= if SUITE-TRUE exit then
   s" c-call-emitter-shape" SUITE-LABEL= if SUITE-TRUE exit then
   s" signature-scan-emitter-shape" SUITE-LABEL= if SUITE-TRUE exit then
   s" compiler-dispatch-shape" SUITE-LABEL= if SUITE-TRUE exit then
   s" stdlib-batch-fixtures" SUITE-LABEL= if SUITE-TRUE exit then
   s" bootstrap-helper-fixtures" SUITE-LABEL= if SUITE-TRUE exit then
   SUITE-FALSE ;

: SUITE-RUN? ( -- bool )
   SUITE-ALL? if SUITE-TRUE exit then
   SUITE-WARM? if SUITE-FALSE exit then
   SUITE-LINT? if SUITE-TRUE exit then
   SUITE-TOOL? if SUITE-TRUE exit then
   SUITE-CHECK-CLI? if SUITE-TRUE exit then
   SUITE-TAIL? if SUITE-TRUE exit then
   SUITE-FALSE ;

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
   s" bin/hb" SUITE-TIMEOUT-MS label labelu SUITE-RUN-ENV-ASYNC ;

: SUITE-HB-RUN-STDIN ( ptr u8 n ptr u8 n -- ) {: in:ptr inu label:ptr labelu :}
   GT-POOL-DRAIN
   label labelu GT-PROGRESS-RUN
   s" bin/hb" in inu SUITE-TIMEOUT-MS label labelu SUITE-RUN-STDIN
   label labelu SUITE-EXPECT-OK
   label labelu GT-PROGRESS-PASS ;

: TEST-SUITE ( -- )
   SUITE-PARSE-LABEL
   SUITE-HB
   SUITE-PARSE-ARGS
   SUITE-RUN? 0= if exit then
   SUITE-LABEL$ SUITE-HB-RUN ;

: TEST-SUITE-STDIN ( -- )
   SUITE-PARSE-LABEL
   SUITE-PARSE-STDIN
   SUITE-HB
   SUITE-PARSE-ARGS
   SUITE-RUN? 0= if exit then
   SUITE-STDIN$ SUITE-LABEL$ SUITE-HB-RUN-STDIN ;

: TEST-SUITE-IMGDUMP ( -- )
   SUITE-PARSE-LABEL
   SUITE-HB
   SUITE-ARG-TARGET-LAYOUT
   SUITE-PARSE-ARGS
   SUITE-RUN? 0= if exit then
   SUITE-LABEL$ SUITE-HB-RUN ;

: SUITE-TOOL-BASE ( -- )
   s" tools/date.f" SUITE-ARG+
   s" lib/errors.f" SUITE-ARG+
   s" lib/string.f" SUITE-ARG+
   s" lib/test.f" SUITE-ARG+
   s" lib/memory.f" SUITE-ARG+
   s" lib/vector.f" SUITE-ARG+
   s" lib/fs.f" SUITE-ARG+
   s" lib/fs-mutate.f" SUITE-ARG+
   s" lib/process.f" SUITE-ARG+
   s" lib/process-argv.f" SUITE-ARG+
   s" lib/process-env.f" SUITE-ARG+
   s" tools/lint/text.f" SUITE-ARG+
   s" tools/lint/intern.f" SUITE-ARG+
   s" tools/lint/token.f" SUITE-ARG+
   s" tools/lint/lib.f" SUITE-ARG+
   s" tools/lint/json-writer.f" SUITE-ARG+
   s" tools/lint/source-lex.f" SUITE-ARG+
   s" tools/json.f" SUITE-ARG+
   s" tools/diag-origin-core.f" SUITE-ARG+
   s" tools/json-only-core.f" SUITE-ARG+
   s" tools/argv.f" SUITE-ARG+
   s" tools/warm-run.f" SUITE-ARG+ ;

: TEST-TOOL-SUITE ( -- )
   SUITE-PARSE-LABEL
   SUITE-HB
   SUITE-TOOL-BASE
   SUITE-PARSE-ARGS
   SUITE-RUN? 0= if exit then
   SUITE-LABEL$ SUITE-HB-RUN ;

: GATE-STDLIB-MAIN ( -- )
   SUITE-CHECK-ARGS
   GT-RESET
   GT-POOL-RESET
   SUITE-WARM-PREPARE ;

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
   tools/date.f lib/errors.f lib/string.f lib/memory.f lib/fs.f tools/lint/text.f
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
   lib/errors.f lib/string.f lib/memory.f lib/fs.f lib/process.f lib/process-argv.f
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

TEST-TOOL-SUITE tool-boundary-trust
   tools/trust-lint-test.f
   tools/aot-call-report.f tools/aot-call-report-test.f
;TEST-SUITE

TEST-TOOL-SUITE tool-boundary-check-repair
   tools/check-all-errors-test.f tools/repair-packet-test.f
;TEST-SUITE

TEST-TOOL-SUITE tool-boundary-doc-public
   tools/public-signatures-test.f tools/stale-status-lint-test.f
   tools/repair-schema-doc-test.f tools/examples-test.f
;TEST-SUITE

TEST-TOOL-SUITE tool-boundary-lints
   tools/repl-lint-test.f tools/diag-origin-test.f tools/aot-lint-test.f
   tools/signature-lint-test.f tools/checked-boundary-lint-test.f
   tools/bundle-lib-test.f tools/json-only-test.f
;TEST-SUITE

TEST-SUITE check-cli-boundary
   tools/date.f lib/errors.f lib/string.f lib/test.f lib/memory.f lib/vector.f lib/fs.f lib/fs-mutate.f
   lib/process.f lib/process-argv.f lib/source.f tools/lint/text.f
   tools/lint/token.f tools/lint/lib.f tools/lint/json-writer.f
   tools/lint/source-lex.f tools/diag-origin-core.f tools/json.f
   tools/json-only-core.f tools/signature-lint-core.f
   tools/checked-boundary-lint-core.f tools/trust-lint-core.f
   tools/check-all-errors-core.f tools/argv.f tools/warm-run.f
   tools/check-test.f
;TEST-SUITE

TEST-SUITE streaming-sha256
   lib/errors.f lib/string.f lib/test.f lib/fs.f lib/fs-mutate.f
   src/core/sha256.f tools/sha256-file-test.f
;TEST-SUITE

TEST-SUITE string-helpers
   lib/errors.f lib/string.f lib/string-test.f
;TEST-SUITE

TEST-SUITE ffi-cabi
   lib/errors.f lib/string.f lib/test.f lib/ffi.f lib/ffi-test.f
;TEST-SUITE

TEST-SUITE float-parse
   lib/errors.f lib/string.f lib/test.f lib/float.f lib/float-test.f
;TEST-SUITE

TEST-SUITE fmt-numbers
   lib/errors.f lib/string.f lib/test.f lib/float.f lib/fmt.f lib/fmt-test.f
;TEST-SUITE

TEST-SUITE float-sort
   lib/errors.f lib/test.f lib/sort.f lib/sort-test.f
;TEST-SUITE

TEST-SUITE float-stats
   lib/errors.f lib/test.f lib/sort.f lib/stats.f lib/stats-test.f
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

TEST-SUITE ptx-stdlib
   lib/errors.f lib/test.f lib/ptx.f lib/ptx-test.f
;TEST-SUITE

TEST-SUITE ptx-toolchain
   lib/errors.f lib/string.f lib/test.f lib/fs.f lib/process.f
   lib/process-argv.f lib/process-env.f src/arch/ptx/emit.f
   tools/ptx/saxpy-test.f
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

TEST-SUITE bootstrap-helper-fixtures
   lib/errors.f lib/string.f lib/test.f lib/memory.f lib/fs.f
   lib/fs-mutate.f lib/process.f lib/process-argv.f lib/process-env.f
   lib/source.f lib/build.f lib/codesign.f tools/build-fixpoint.f
   tools/warm-image-lib.f tools/bootstrap-codegen-test.f
   bootstrap/cg/asm-checked.fs tools/asm-checked-test.f
   src/os/image-bytes.f tools/image-bytes-test.f
   tools/warm-image-test.f
;TEST-SUITE

TEST-SUITE build-fixpoint-fixtures
   lib/errors.f lib/string.f lib/test.f lib/memory.f lib/fs.f
   lib/fs-mutate.f lib/process.f lib/process-argv.f lib/process-env.f
   lib/source.f lib/build.f lib/codesign.f tools/build-fixpoint.f
   tools/build-fixpoint-test.f
;TEST-SUITE

TEST-SUITE hb-build-fixtures
   lib/errors.f lib/string.f lib/test.f lib/memory.f lib/fs.f
   lib/fs-mutate.f lib/process.f lib/process-argv.f lib/process-env.f
   lib/source.f lib/build.f lib/codesign.f tools/build-fixpoint.f
   tools/warm-run.f tools/hb-build-lib.f tools/hb-build-test.f
   lib/codesign-test.f
;TEST-SUITE

GT-POOL-DRAIN
SUITE-CLEANUP
s" PASS: native lint/stdlib gate phase" type cr
