\ gate-stdlib.f - Habu-specific adapter for the native lint/stdlib test phase.
\
\ Load after lib/errors.f, lib/string.f, lib/fs.f, lib/fs-mutate.f,
\ lib/process.f, lib/process-argv.f, lib/process-env.f, lib/test/runner.f,
\ test/gate-pool.f, lib/test.f, and lib/content-key.f.

require lib/test.f

120000 constant SUITE-TIMEOUT-MS
64 constant SUITE-USAGE-RC
0 constant SUITE-ALL-ID
1 constant SUITE-WARM-ID
2 constant SUITE-LINT-ID
3 constant SUITE-TOOL-ID
4 constant SUITE-CHECK-CLI-ID
5 constant SUITE-TAIL-ID
6 constant SUITE-LINT-TOOLS-ID
7 constant SUITE-LINT-LIBS-ID
8 constant SUITE-LINT-ARTIFACTS-ID
9 constant SUITE-LINT-MANIFEST-ID

create SUITE-WARM-BUF FS-PATH-CAP allot
create SUITE-WARM-TRUST-BUF FS-PATH-CAP allot
create SUITE-WARM-CAND-BUF FS-PATH-CAP allot
create SUITE-WARM-CAND-TRUST-BUF FS-PATH-CAP allot
create SUITE-WARM-OUT GT-OUT-CAP allot
create SUITE-WARM-ERR GT-ERR-CAP allot
create SUITE-WARM-STAMP-BUF FS-PATH-CAP allot
create SUITE-KEY-HEX 80 allot
create SUITE-STAMP-RD 80 allot
variable SUITE-WARM-STAMP-U
variable SUITE-WARM-U
variable SUITE-WARM-TRUST-U
variable SUITE-WARM-CAND-U
variable SUITE-WARM-CAND-TRUST-U
variable SUITE-OWN-ROOT
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
   s" usage: test/gate-stdlib.f [warm|lint|lint-tools|lint-manifest|lint-artifacts|lint-libs|tool|check-cli|tail] [--pool-slots N] [--timings]" SUITE-USAGE-RC die ;

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
   SUITE-ARG$ s" warm" STR= if SUITE-WARM-ID SUITE-SLICE! SUITE-TRUE exit then
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
   s" HABU_WARM_TOOLS" >LEN SUITE-WARM-BUF SUITE-WARM-U @ >LEN PROC-ENV+
   s" HABU_WARM_TOOLS_TRUST" >LEN SUITE-WARM-TRUST-BUF SUITE-WARM-TRUST-U @ >LEN PROC-ENV+
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

: SUITE-SUFFIX! ( ptr u8 n ptr u8 n ptr u8 ptr n -- )
   {: a:ptr u:n suf:ptr su:n dst:ptr lenp:ptr :}
   u su + FS-PATH-CAP > if E-FS-PATH throw then
   a dst u BYTE-COPY
   suf dst u + su BYTE-COPY
   u su + lenp ! ;

: SUITE-WARM$ ( -- ptr u8 n )
   SUITE-WARM-BUF SUITE-WARM-U @ ;

: SUITE-WARM-TRUST$ ( -- ptr u8 n )
   SUITE-WARM-TRUST-BUF SUITE-WARM-TRUST-U @ ;

: SUITE-WARM-CAND$ ( -- ptr u8 n )
   SUITE-WARM-CAND-BUF SUITE-WARM-CAND-U @ ;

: SUITE-WARM-CAND-TRUST$ ( -- ptr u8 n )
   SUITE-WARM-CAND-TRUST-BUF SUITE-WARM-CAND-TRUST-U @ ;

\ Tools-warm root: the gate-supplied HABU_GATE_WARM_ROOT when this runs under
\ test/run.f, else an owned temp. Must match TR-TOOLS-PATHS in run.f so the baked
\ image and HABU_WARM_TOOLS resolve to the same place.
: SUITE-SET-ROOT ( -- )
   0 SUITE-OWN-ROOT !
   s" HABU_GATE_WARM_ROOT" GETENV dup 0= 0= if GT-COPY-ROOT! exit then 2drop
   CLEANUP-RESET
   s" hb-stdlib-warm" TMPDIR-MKDIR GT-COPY-ROOT!
   GT-ROOT CLEANUP-TREE+
   -1 SUITE-OWN-ROOT ! ;

: SUITE-WARM-PATHS ( -- )
   GT-ROOT s" hb-tools-warm" SUITE-WARM-BUF JOIN-PATH SUITE-WARM-U !
   SUITE-WARM$ s" .trust.f" SUITE-WARM-TRUST-BUF SUITE-WARM-TRUST-U SUITE-SUFFIX!
   SUITE-WARM$ s" .new" SUITE-WARM-CAND-BUF SUITE-WARM-CAND-U SUITE-SUFFIX!
   SUITE-WARM-CAND$ s" .trust.f" SUITE-WARM-CAND-TRUST-BUF SUITE-WARM-CAND-TRUST-U SUITE-SUFFIX! ;

: SUITE-KEY-FILE+ ( ptr u8 n -- )
   CK-FILE+ ;

: SUITE-SNAPSHOT-LINUX-KEY ( -- )
   s" target:linux-aarch64" CK-TEXT+
   s" src/os/linux/layout.f" SUITE-KEY-FILE+
   s" src/os/linux/elf.f" SUITE-KEY-FILE+
   s" src/os/linux/sign.f" SUITE-KEY-FILE+ ;

: SUITE-SNAPSHOT-MACOS-KEY ( -- )
   s" target:macos-aarch64" CK-TEXT+
   s" src/os/macos/layout.f" SUITE-KEY-FILE+
   s" src/os/macos/macho.f" SUITE-KEY-FILE+
   s" src/os/macos/sign2.f" SUITE-KEY-FILE+ ;

: SUITE-SNAPSHOT-TARGET-KEY ( -- )
   HB-TARGET-LINUX? if SUITE-SNAPSHOT-LINUX-KEY exit then
   HB-TARGET-MACOS? if SUITE-SNAPSHOT-MACOS-KEY exit then
   s" gate-stdlib warm cache unknown target" SUITE-USAGE-RC die ;

: SUITE-SNAPSHOT-BUILDER-KEY ( -- )
   s" src/os/image-bytes.f" SUITE-KEY-FILE+
   SUITE-SNAPSHOT-TARGET-KEY
   s" src/habu/snap-lib.f" SUITE-KEY-FILE+
   s" src/habu/snap.f" SUITE-KEY-FILE+ ;

\ Content key over the warm image's inputs: the compiler (bin/hb), the baker, and
\ every baked tool source plus baker-side trust-export dependencies. KEEP THE
\ baked-source portion IN SYNC with SUITE-WARM-SUPPORT-ARGV (a drift means a
\ stale image could be reused). A persistent warm image is reused only when this
\ key matches, so any input change forces a rebake.
: SUITE-WARM-KEY! ( -- )
   CK-RESET
   s" hb-tools-warm-cache-v4" CK-TEXT+
   s" bin/hb" SUITE-KEY-FILE+
   s" test/gate-stats.f" SUITE-KEY-FILE+
   s" tools/warm-image-lib.f" SUITE-KEY-FILE+
   s" tools/warm-image-gate-stats.f" SUITE-KEY-FILE+
   s" tools/warm-image.f" SUITE-KEY-FILE+
   SUITE-SNAPSHOT-BUILDER-KEY
   s" tools/public-signatures-core.f" SUITE-KEY-FILE+
   s" tools/public-signatures.f" SUITE-KEY-FILE+
   s" tools/date.f" SUITE-KEY-FILE+
   s" lib/errors.f" SUITE-KEY-FILE+
   s" lib/string.f" SUITE-KEY-FILE+
   s" lib/memory.f" SUITE-KEY-FILE+
   s" lib/vector.f" SUITE-KEY-FILE+
   s" lib/fs.f" SUITE-KEY-FILE+
   s" lib/fs-mutate.f" SUITE-KEY-FILE+
   s" lib/process.f" SUITE-KEY-FILE+
   s" lib/process-argv.f" SUITE-KEY-FILE+
   s" lib/process-env.f" SUITE-KEY-FILE+
   s" lib/source.f" SUITE-KEY-FILE+
   s" lib/codesign.f" SUITE-KEY-FILE+
   s" tools/lint/text.f" SUITE-KEY-FILE+
   s" tools/lint/intern.f" SUITE-KEY-FILE+
   s" tools/lint/token.f" SUITE-KEY-FILE+
   s" tools/lint/lib.f" SUITE-KEY-FILE+
   s" tools/lint/json-writer.f" SUITE-KEY-FILE+
   s" tools/lint/source-lex.f" SUITE-KEY-FILE+
   s" tools/argv.f" SUITE-KEY-FILE+
   s" tools/diag-origin-core.f" SUITE-KEY-FILE+
   s" tools/json.f" SUITE-KEY-FILE+
   s" tools/gate-json-assert-core.f" SUITE-KEY-FILE+
   s" tools/json-only-core.f" SUITE-KEY-FILE+
   s" tools/aot-lint-core.f" SUITE-KEY-FILE+
   s" tools/signature-lint-core.f" SUITE-KEY-FILE+
   s" tools/checked-boundary-lint-core.f" SUITE-KEY-FILE+
   s" tools/reserved-name-lint-core.f" SUITE-KEY-FILE+
   s" tools/duplicate-definition-lint-core.f" SUITE-KEY-FILE+
   s" tools/typed-local-diff-lint-core.f" SUITE-KEY-FILE+
   s" tools/trust-lint-core.f" SUITE-KEY-FILE+
   s" tools/check-all-errors-core.f" SUITE-KEY-FILE+
   s" src/habu/verify-source.f" SUITE-KEY-FILE+
   SUITE-KEY-HEX CK-FINAL-HEX ;

: SUITE-WARM-STAMP$ ( -- ptr u8 n )
   SUITE-WARM$ s" .stamp" SUITE-WARM-STAMP-BUF SUITE-WARM-STAMP-U SUITE-SUFFIX!
   SUITE-WARM-STAMP-BUF SUITE-WARM-STAMP-U @ ;

\ A cached warm image is valid only if image + trust exist AND the content stamp
\ matches the current key. SUITE-WARM-KEY! must run before this check.
: SUITE-WARM-CACHED? ( -- bool )
   SUITE-WARM$ EXECUTABLE? 0= if SUITE-FALSE exit then
   SUITE-WARM-TRUST$ FILE? 0= if SUITE-FALSE exit then
   SUITE-WARM-STAMP$ FILE? 0= if SUITE-FALSE exit then
   SUITE-WARM-STAMP$ SUITE-STAMP-RD 80 READ-ALL
   dup 64 <> if drop SUITE-FALSE exit then
   SUITE-STAMP-RD swap SUITE-KEY-HEX 64 STR= ;

: SUITE-WARM-TOOL-ARGV ( -- )
   PROC-ARGV-ENV-RESET
   s" --load" SUITE-ARG+
   s" tools/warm-image-gate-stats.f" SUITE-ARG+
   s" tools/warm-image.f" SUITE-ARG+
   s" --" SUITE-ARG+
   SUITE-WARM-CAND$ SUITE-ARG+ ;

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
   s" tools/argv.f" SUITE-ARG+
   s" tools/diag-origin-core.f" SUITE-ARG+
   s" tools/json.f" SUITE-ARG+
   s" tools/gate-json-assert-core.f" SUITE-ARG+
   s" tools/json-only-core.f" SUITE-ARG+
   s" tools/aot-lint-core.f" SUITE-ARG+
   s" tools/signature-lint-core.f" SUITE-ARG+
   s" tools/checked-boundary-lint-core.f" SUITE-ARG+
   s" tools/reserved-name-lint-core.f" SUITE-ARG+
   s" tools/duplicate-definition-lint-core.f" SUITE-ARG+
   s" tools/typed-local-diff-lint-core.f" SUITE-ARG+
   s" tools/trust-lint-core.f" SUITE-ARG+
   s" tools/check-all-errors-core.f" SUITE-ARG+ ;

: SUITE-WARM-PRINT ( n n -- ) {: outu:n erru:n :}
   SUITE-WARM-OUT outu type
   SUITE-WARM-ERR erru type ;

: SUITE-OUTCOME. ( n -- ) {: kind:n :}
   kind case
      PROC-OUTCOME-EXIT of s" exit" type endof
      PROC-OUTCOME-SIGNAL of s" signal" type endof
      PROC-OUTCOME-TIMEOUT of s" timeout" type endof
      s" unknown" type
   endcase ;

: SUITE-WARM-OK? ( n n -- bool ) {: kind:n code:n :}
   kind PROC-OUTCOME-EXIT =
   code 0= and ;

: SUITE-REMOVE-FILE? ( ptr u8 n -- ) {: a:ptr u:n :}
   a u FILE? if a u REMOVE-FILE then ;

: SUITE-WARM-CLEAN-CANDIDATE ( -- )
   SUITE-WARM-CAND$ SUITE-REMOVE-FILE?
   SUITE-WARM-CAND-TRUST$ SUITE-REMOVE-FILE? ;

: SUITE-WARM-DROP-STAMP ( -- )
   SUITE-WARM-STAMP$ SUITE-REMOVE-FILE? ;

: SUITE-WARM-PUBLISH ( -- )
   SUITE-WARM-DROP-STAMP
   SUITE-WARM-CAND-TRUST$ SUITE-WARM-TRUST$ RENAME-FILE
   SUITE-WARM-CAND$ SUITE-WARM$ RENAME-FILE
   SUITE-WARM-STAMP$ SUITE-KEY-HEX 64 ATOMIC-WRITE-FILE ;

: SUITE-WARM-FAIL ( len len n n -- ) {: outu:len erru:len kind:n code:n :}
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
   s" warm-build" GS-EVENT
   SUITE-WARM-TOOL-ARGV
   SUITE-WARM-SUPPORT-ARGV
   PROC-ENV-RESET
   PROC-ENV-INHERIT-MISSING
   s" bin/hb" >LEN SUITE-WARM-OUT GT-OUT-CAP >LEN SUITE-WARM-ERR GT-ERR-CAP >LEN
   SUITE-TIMEOUT-MS >MS RUN-ARGV-ENV-CAPTURE-OUTCOME
   {: outu:len erru:len kind:n code:n :}
   kind code SUITE-WARM-OK? 0= if
      SUITE-WARM-CLEAN-CANDIDATE
      outu erru kind code SUITE-WARM-FAIL
   then ;

: SUITE-WARM-PREPARE ( -- )
   SUITE-SET-ROOT
   SUITE-WARM-PATHS
   SUITE-WARM-KEY!
   SUITE-WARM-CACHED? if s" warm-cache-hit" GS-EVENT exit then
   s" warm-cache-miss" GS-EVENT
   SUITE-WARM-CLEAN-CANDIDATE
   SUITE-WARM-RUN
   SUITE-WARM-PUBLISH ;

: SUITE-CLEANUP ( -- )
   SUITE-OWN-ROOT @ if CLEANUP-RUN then ;

: SUITE-LABEL$ ( -- ptr u8 n )
   TEST:LABEL$ ;

: SUITE-LABEL= ( ptr u8 n -- bool ) {: a:ptr u:n :}
   a u TEST:LABEL= ;

: SUITE-ALL? ( -- bool )
   SUITE-SLICE @ SUITE-ALL-ID = ;

: SUITE-WARM? ( -- bool )
   SUITE-SLICE @ SUITE-WARM-ID = ;

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
   SUITE-WARM? if SUITE-FALSE exit then
   SUITE-LINT? if SUITE-TRUE exit then
   SUITE-TOOL? if SUITE-TRUE exit then
   SUITE-CHECK-CLI? if SUITE-TRUE exit then
   SUITE-TAIL? if SUITE-TRUE exit then
   SUITE-FALSE ;

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

: SUITE-HB$ ( -- ptr u8 n )
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
   SUITE-INSTALL-POOL-HOOKS
   SUITE-WARM-PREPARE ;

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
