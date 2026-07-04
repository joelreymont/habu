\ gate-stdlib.f - Habu-specific adapter for the native lint/stdlib test phase.
\
\ Load after lib/errors.f, lib/string.f, lib/fs.f, lib/fs-mutate.f,
\ lib/process.f, lib/process-argv.f, lib/process-env.f, lib/test/runner.f,
\ test/gate-pool.f, lib/test.f, and lib/content-key.f.

require lib/test.f

120000 constant SUITE-TIMEOUT-MS
64 constant SUITE-USAGE-RC
0 constant SUITE-ALL-ID
2 constant SUITE-LINT-ID
3 constant SUITE-TOOL-ID
4 constant SUITE-CHECK-CLI-ID
5 constant SUITE-TAIL-ID
6 constant SUITE-LINT-TOOLS-ID
7 constant SUITE-LINT-LIBS-ID
8 constant SUITE-LINT-ARTIFACTS-ID
9 constant SUITE-LINT-MANIFEST-ID

variable SUITE-SLICE
variable SUITE-SKIP-TOOL-LINTS
variable SUITE-SKIP-TOOL-REPAIR
variable SUITE-SKIP-TOOL-DOC
variable SUITE-SKIP-TOOL-TYPED
variable SUITE-ARG-I
variable SUITE-SLICE-SEEN
variable SUITE-TIMINGS

: SUITE-TRUE ( -- bool )
   0 0= ;

: SUITE-FALSE ( -- bool )
   0 0= 0= ;

: SUITE-USAGE ( -- )
   s" usage: test/gate-stdlib.f [lint|lint-tools|lint-manifest|lint-artifacts|lint-libs|tool|check-cli|tail] [--pool-slots N] [--timings]" SUITE-USAGE-RC die ;

: SUITE-ARG$ ( -- ptr u8 n )
   SUITE-ARG-I @ SCRIPT-ARGV$ ;

: SUITE-ARG-VALUE$ ( -- ptr u8 n )
   SUITE-ARG-I @ 1+ SCRIPT-ARGC >= if SUITE-USAGE then
   SUITE-ARG-I @ 1+ SCRIPT-ARGV$ ;

: SUITE-POS-NUM ( ptr u8 n -- n )
   STR>NUMBER? 0= if drop SUITE-USAGE then
   dup 1 < if drop SUITE-USAGE then ;

: SUITE-ADVANCE ( n -- )
   SUITE-ARG-I @ + SUITE-ARG-I ! ;

: SUITE-SLICE! ( n -- )
   SUITE-SLICE ! ;

: SUITE-SKIP-TOOL-LINTS! ( -- )
   -1 SUITE-SKIP-TOOL-LINTS ! ;

: SUITE-SKIP-TOOL-REPAIR! ( -- )
   -1 SUITE-SKIP-TOOL-REPAIR ! ;

: SUITE-SKIP-TOOL-DOC! ( -- )
   -1 SUITE-SKIP-TOOL-DOC ! ;

: SUITE-SKIP-TOOL-TYPED! ( -- )
   -1 SUITE-SKIP-TOOL-TYPED ! ;

: SUITE-SKIP-TOOL-SEMANTIC! ( -- )
   SUITE-SKIP-TOOL-LINTS!
   SUITE-SKIP-TOOL-REPAIR!
   SUITE-SKIP-TOOL-DOC!
   SUITE-SKIP-TOOL-TYPED! ;

: SUITE-POOL-OPT ( -- )
   SUITE-ARG-VALUE$ SUITE-POS-NUM GT-POOL-SLOTS!
   2 SUITE-ADVANCE ;

: SUITE-TIMINGS-OPT ( -- )
   -1 SUITE-TIMINGS !
   1 SUITE-ADVANCE ;

: SUITE-SLICE-ARG? ( -- bool )
   SUITE-ARG$ s" lint" STR= if SUITE-LINT-ID SUITE-SLICE! SUITE-TRUE exit then
   SUITE-ARG$ s" lint-tools" STR= if SUITE-LINT-TOOLS-ID SUITE-SLICE! SUITE-TRUE exit then
   SUITE-ARG$ s" lint-manifest" STR= if SUITE-LINT-MANIFEST-ID SUITE-SLICE! SUITE-TRUE exit then
   SUITE-ARG$ s" lint-artifacts" STR= if SUITE-LINT-ARTIFACTS-ID SUITE-SLICE! SUITE-TRUE exit then
   SUITE-ARG$ s" lint-libs" STR= if SUITE-LINT-LIBS-ID SUITE-SLICE! SUITE-TRUE exit then
   SUITE-ARG$ s" tool" STR= if SUITE-TOOL-ID SUITE-SLICE! SUITE-TRUE exit then
   SUITE-ARG$ s" check-cli" STR= if SUITE-CHECK-CLI-ID SUITE-SLICE! SUITE-TRUE exit then
   SUITE-ARG$ s" tail" STR= if SUITE-TAIL-ID SUITE-SLICE! SUITE-TRUE exit then
   SUITE-FALSE ;

: SUITE-SLICE-OPT ( -- )
   SUITE-SLICE-SEEN @ if SUITE-USAGE then
   SUITE-SLICE-ARG? 0= if SUITE-USAGE then
   -1 SUITE-SLICE-SEEN !
   1 SUITE-ADVANCE ;

: SUITE-PARSE-ARG ( -- )
   SUITE-ARG$ s" --pool-slots" STR= if SUITE-POOL-OPT exit then
   SUITE-ARG$ s" --timings" STR= if SUITE-TIMINGS-OPT exit then
   SUITE-SLICE-OPT ;

: SUITE-PARSE-SLICE ( -- )
   SUITE-ALL-ID SUITE-SLICE!
   0 SUITE-SLICE-SEEN !
   0 SUITE-TIMINGS !
   0 SUITE-ARG-I !
   begin SUITE-ARG-I @ SCRIPT-ARGC < while
      SUITE-PARSE-ARG
   repeat ;

: SUITE-CHECK-ARGS ( -- )
   SUITE-PARSE-SLICE ;

: SUITE-ENV ( -- )
   PROC-ENV-RESET
   PROC-ENV-INHERIT-MISSING ;

: SUITE-RUN-ENV ( ptr u8 n n ptr u8 n -- ) {: path:ptr pathu:n timeout:n label:ptr labelu:n :}
   SUITE-ENV
   path pathu >LEN PROC-ARGV-CHECK-PATH
   PROC-CAPTURE-RESET
   timeout >MS PROC-CAPTURE-DEADLINE!
   PROC-SETUP-CAPTURE-FDS
   path pathu >LEN PROC-ARGV-PREPARE PROC-ENV-PREPARE PROC-SPAWN-ARGV-ENV-CAPTURE
   label labelu GT-PROGRESS-CAPTURE
   PROC-CLOSE-CAPTURE-FDS ;

: SUITE-RUN-STDIN ( ptr u8 n ptr u8 n n ptr u8 n -- ) {: path:ptr pathu:n in:ptr inu:n timeout:n label:ptr labelu:n :}
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

: SUITE-RUN-ENV-ASYNC ( ptr u8 n n ptr u8 n -- ) {: path:ptr pathu:n timeout:n label:ptr labelu:n :}
   SUITE-ENV
   path pathu label labelu timeout GT-POOL-START ;

: SUITE-FAIL ( ptr u8 n -- ) {: label:ptr labelu:n :}
   s" FAIL: " type label labelu type cr
   s" rc: " type GT-RC@ . cr
   GT-OUT$ type
   GT-ERR$ type
   s" gate stdlib phase failed" 1 die ;

: SUITE-EXPECT-OK ( ptr u8 n -- ) {: label:ptr labelu:n :}
   GT-RC@ 0 <> if label labelu SUITE-FAIL then ;

: SUITE-ARG+ ( ptr u8 n -- )
    >LEN PROC-ARGV+ ;

: SUITE-CLEANUP ( -- )
   ;

: SUITE-LABEL$ ( -- ptr u8 n )
   TEST:LABEL$ ;

: SUITE-LABEL= ( ptr u8 n -- bool ) {: a:ptr u:n :}
   a u TEST:LABEL= ;

: SUITE-ALL? ( -- bool )
   SUITE-SLICE @ SUITE-ALL-ID = ;

: SUITE-LINT-TOOLS-LABEL? ( -- bool )
   s" shadow-lint" SUITE-LABEL= if SUITE-TRUE exit then
   s" clobber-lint" SUITE-LABEL= if SUITE-TRUE exit then
   s" clobber-lint-fixtures" SUITE-LABEL= if SUITE-TRUE exit then
   s" repl-lint" SUITE-LABEL= if SUITE-TRUE exit then
   s" trust-lint" SUITE-LABEL= if SUITE-TRUE exit then
   s" stale-status-lint" SUITE-LABEL= if SUITE-TRUE exit then
   s" host-lint" SUITE-LABEL= if SUITE-TRUE exit then
   s" parallel-agent-lint" SUITE-LABEL= if SUITE-TRUE exit then
   s" filemap-lint" SUITE-LABEL= if SUITE-TRUE exit then
   s" stdin-closure-lint" SUITE-LABEL= if SUITE-TRUE exit then
   s" gate-stats" SUITE-LABEL= if SUITE-TRUE exit then
   s" dot-dep-lint" SUITE-LABEL= if SUITE-TRUE exit then
   s" dot-dep-lint-fixtures" SUITE-LABEL= if SUITE-TRUE exit then
   s" maki-dep-lint" SUITE-LABEL= if SUITE-TRUE exit then
   s" maki-dep-lint-fixtures" SUITE-LABEL= if SUITE-TRUE exit then
   s" host-lint-fixtures" SUITE-LABEL= if SUITE-TRUE exit then
   SUITE-FALSE ;

: SUITE-LINT-ARTIFACTS-LABEL? ( -- bool )
   s" imgdump-compare" SUITE-LABEL= if SUITE-TRUE exit then
   s" imagedisasm-tool" SUITE-LABEL= if SUITE-TRUE exit then
   SUITE-FALSE ;

: SUITE-LINT-MANIFEST-LABEL? ( -- bool )
   s" stdlib-manifest" SUITE-LABEL= ;

: SUITE-LINT-LIBS-LABEL? ( -- bool )
   s" string-helpers" SUITE-LABEL= if SUITE-TRUE exit then
   s" array-helpers" SUITE-LABEL= if SUITE-TRUE exit then
   s" table-stdlib" SUITE-LABEL= if SUITE-TRUE exit then
   s" regex-stdlib" SUITE-LABEL= if SUITE-TRUE exit then
   s" map-stdlib" SUITE-LABEL= if SUITE-TRUE exit then
   s" ffi-abi" SUITE-LABEL= if SUITE-TRUE exit then
   s" ptx-stdlib" SUITE-LABEL= if SUITE-TRUE exit then
   s" ptx-tile-loop-neg" SUITE-LABEL= if SUITE-TRUE exit then
   s" ptx-tile-smem-neg" SUITE-LABEL= if SUITE-TRUE exit then
   s" ptx-tile-acc-neg" SUITE-LABEL= if SUITE-TRUE exit then
   s" ptx-gemm-checked-neg" SUITE-LABEL= if SUITE-TRUE exit then
   s" ptx-toolchain" SUITE-LABEL= if SUITE-TRUE exit then
   SUITE-FALSE ;

: SUITE-LINT? ( -- bool )
   SUITE-SLICE @ case
      SUITE-LINT-ID of
         SUITE-LINT-TOOLS-LABEL? if SUITE-TRUE exit then
         SUITE-LINT-MANIFEST-LABEL? if SUITE-TRUE exit then
         SUITE-LINT-ARTIFACTS-LABEL? if SUITE-TRUE exit then
         SUITE-LINT-LIBS-LABEL?
      endof
      SUITE-LINT-TOOLS-ID of SUITE-LINT-TOOLS-LABEL? endof
      SUITE-LINT-MANIFEST-ID of SUITE-LINT-MANIFEST-LABEL? endof
      SUITE-LINT-ARTIFACTS-ID of SUITE-LINT-ARTIFACTS-LABEL? endof
      SUITE-LINT-LIBS-ID of SUITE-LINT-LIBS-LABEL? endof
      SUITE-FALSE swap
   endcase ;

: SUITE-TOOL? ( -- bool )
   SUITE-SLICE @ SUITE-TOOL-ID <> if SUITE-FALSE exit then
   s" tool-boundary-trust" SUITE-LABEL= if SUITE-TRUE exit then
   s" tool-boundary-check-repair" SUITE-LABEL= if
      SUITE-SKIP-TOOL-REPAIR @ 0= if SUITE-TRUE exit then
      SUITE-FALSE exit
   then
   s" tool-boundary-doc-public" SUITE-LABEL= if
      SUITE-SKIP-TOOL-DOC @ 0= if SUITE-TRUE exit then
      SUITE-FALSE exit
   then
   s" tool-boundary-lints" SUITE-LABEL= if
      SUITE-SKIP-TOOL-LINTS @ 0= if SUITE-TRUE exit then
      SUITE-FALSE exit
   then
   s" tool-boundary-typed-local" SUITE-LABEL= if
      SUITE-SKIP-TOOL-TYPED @ 0= if SUITE-TRUE exit then
      SUITE-FALSE exit
   then
   SUITE-FALSE ;

: SUITE-CHECK-CLI? ( -- bool )
   SUITE-SLICE @ SUITE-CHECK-CLI-ID <> if SUITE-FALSE exit then
   s" check-cli-boundary" SUITE-LABEL= ;

: SUITE-TAIL? ( -- bool )
   SUITE-SLICE @ SUITE-TAIL-ID <> if SUITE-FALSE exit then
   s" source-stdlib-stdin" SUITE-LABEL= if SUITE-TRUE exit then
   s" argv-stdlib-mocks" SUITE-LABEL= if SUITE-TRUE exit then
   s" argv-stdlib-script-args" SUITE-LABEL= if SUITE-TRUE exit then
   s" stdlib-source-default" SUITE-LABEL= if SUITE-TRUE exit then
   SUITE-FALSE ;

: SUITE-RUN? ( -- bool )
   SUITE-ALL? if SUITE-TRUE exit then
   SUITE-LINT? if SUITE-TRUE exit then
   SUITE-TOOL? if SUITE-TRUE exit then
   SUITE-CHECK-CLI? if SUITE-TRUE exit then
   SUITE-TAIL? if SUITE-TRUE exit then
   SUITE-FALSE ;

: SUITE-HB ( -- )
   PROC-ARGV-RESET
   s" --load" SUITE-ARG+ ;

: SUITE-HB$ ( -- ptr u8 n )
   s" HABU_UNDER_TEST" >LEN PROC-ENV-DEFAULT$? if LEN>N exit then
   2drop
   s" HABU_UNDER_TEST" GETENV dup 0= if
      2drop s" bin/hb" exit
   then
   2dup EXECUTABLE? 0= if E-FS-OPEN throw then ;

: SUITE-HB-RUN ( ptr u8 n -- ) {: label:ptr labelu:n :}
   label labelu GS-INNER-HB-EVENT
   label labelu GS-BOUNDARY-EVENT
   SUITE-HB$ SUITE-TIMEOUT-MS label labelu SUITE-RUN-ENV-ASYNC ;

: SUITE-HB-RUN-STDIN ( ptr u8 n ptr u8 n -- ) {: in:ptr inu:n label:ptr labelu:n :}
   GT-POOL-DRAIN
   label labelu GT-PROGRESS-RUN
   label labelu GS-INNER-HB-STDIN-EVENT
   label labelu GS-BOUNDARY-EVENT
   SUITE-HB$ in inu SUITE-TIMEOUT-MS label labelu SUITE-RUN-STDIN
   label labelu SUITE-EXPECT-OK
   label labelu GT-PROGRESS-PASS ;

: SUITE-POOL-PASS-SPAN ( ptr u8 n n -- ) {: label:ptr labelu:n ms:n :}
   label labelu ms GS-SPAN ;

: SUITE-INSTALL-POOL-HOOKS ( -- )
   [: SUITE-POOL-PASS-SPAN ;] is GT-POOL-PASS-HOOK ;

: SUITE-SETUP ( -- )
   SUITE-CHECK-ARGS
   GT-RESET
   GT-POOL-RESET
   SUITE-INSTALL-POOL-HOOKS ;

: SUITE-INSTALL-TEST-HOOKS ( -- )
   [: SUITE-SETUP ;] TEST:SETUP!
   [: SUITE-CLEANUP ;] TEST:TEARDOWN!
   [: GT-POOL-DRAIN ;] TEST:DRAIN!
   [: SUITE-HB ;] TEST:ARGS-BEGIN!
   [: SUITE-ARG+ ;] TEST:ARG+!
   [: SUITE-RUN? ;] TEST:SELECT?!
   [: SUITE-HB-RUN ;] TEST:RUNNER!
   [: SUITE-HB-RUN-STDIN ;] TEST:STDIN-RUNNER! ;

: GATE-STDLIB-MAIN ( -- )
   SUITE-INSTALL-TEST-HOOKS
   TEST:RESET ;
