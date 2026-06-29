\ gate-stdlib.f - checked runner for the default gate lint/stdlib phase.
\
\ Load after lib/errors.f, lib/string.f, lib/fs.f, lib/fs-mutate.f,
\ lib/process.f, lib/process-argv.f, lib/process-env.f, lib/test-runner.f,
\ test/gate-pool.f, and lib/content-key.f.


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
6 constant SUITE-LINT-TOOLS-ID
7 constant SUITE-LINT-LIBS-ID
8 constant SUITE-LINT-ARTIFACTS-ID
9 constant SUITE-LINT-MANIFEST-ID

variable SUITE-DONE
create SUITE-LABEL-BUF SUITE-NAME-CAP allot
create SUITE-STDIN-BUF SUITE-STDIN-CAP allot
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
variable SUITE-LABEL-U
variable SUITE-STDIN-U
variable SUITE-WARM-U
variable SUITE-WARM-TRUST-U
variable SUITE-WARM-CAND-U
variable SUITE-WARM-CAND-TRUST-U
variable SUITE-OWN-ROOT
variable SUITE-SLICE
variable SUITE-SKIP-TOOL-LINTS

: SUITE-TRUE ( -- bool )
   0 0= ;

: SUITE-FALSE ( -- bool )
   0 0= 0= ;

: SUITE-USAGE ( -- )
   s" usage: test/gate-stdlib.f [warm|lint|lint-tools|lint-manifest|lint-artifacts|lint-libs|tool|check-cli|tail]" SUITE-USAGE-RC die ;

: SUITE-ARG0= ( ptr u8 n -- bool )
   0 SCRIPT-ARGV$ STR= ;

: SUITE-SLICE! ( n -- )
   SUITE-SLICE ! ;

: SUITE-SKIP-TOOL-LINTS! ( -- )
   -1 SUITE-SKIP-TOOL-LINTS ! ;

: SUITE-INLINE-WORK ( -- ) ;

: SUITE-PARSE-SLICE ( -- )
   SUITE-ALL-ID SUITE-SLICE!
   SCRIPT-ARGC 0= if exit then
   SCRIPT-ARGC 1 <> if SUITE-USAGE then
   s" warm" SUITE-ARG0= if SUITE-WARM-ID SUITE-SLICE! exit then
   s" lint" SUITE-ARG0= if SUITE-LINT-ID SUITE-SLICE! exit then
   s" lint-tools" SUITE-ARG0= if SUITE-LINT-TOOLS-ID SUITE-SLICE! exit then
   s" lint-manifest" SUITE-ARG0= if SUITE-LINT-MANIFEST-ID SUITE-SLICE! exit then
   s" lint-artifacts" SUITE-ARG0= if SUITE-LINT-ARTIFACTS-ID SUITE-SLICE! exit then
   s" lint-libs" SUITE-ARG0= if SUITE-LINT-LIBS-ID SUITE-SLICE! exit then
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

: SUITE-CHECK-CAP ( n n -- ) {: u:n cap:n :}
   u 0 < if E-STR-BOUNDS throw then
   u cap > if E-STR-CAPACITY throw then ;

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

\ Tools-warm root: the persistent HABU_GATE_WARM_PERSIST dir if the operator opted
\ in (content-stamped, so cross-run reuse is sound), else the per-run shared
\ HABU_GATE_WARM_ROOT, else an owned temp. Must match TR-TOOLS-PATHS in run.f so the
\ baked image and HABU_WARM_TOOLS resolve to the same place.
: SUITE-SET-ROOT ( -- )
   0 SUITE-OWN-ROOT !
   s" HABU_GATE_WARM_PERSIST" GETENV dup 0= 0= if GT-COPY-ROOT! exit then 2drop
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

\ Content key over the warm image's inputs: the compiler (bin/hb), the baker, and
\ every baked tool source plus baker-side trust-export dependencies. KEEP THE
\ baked-source portion IN SYNC with SUITE-WARM-SUPPORT-ARGV (a drift means a
\ stale image could be reused). A persistent warm image is reused only when this
\ key matches, so any input change forces a rebake.
: SUITE-WARM-KEY! ( -- )
   CK-RESET
   s" hb-tools-warm-cache-v3" CK-TEXT+
   s" bin/hb" SUITE-KEY-FILE+
   s" tools/warm-image-lib.f" SUITE-KEY-FILE+
   s" tools/warm-image.f" SUITE-KEY-FILE+
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
   kind PROC-OUTCOME-EXIT = if s" exit" type exit then
   kind PROC-OUTCOME-SIGNAL = if s" signal" type exit then
   kind PROC-OUTCOME-TIMEOUT = if s" timeout" type exit then
   s" unknown" type ;

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

: SUITE-PARSE-NAME ( -- ptr u8 n )
   parse-name dup 0= if 2drop E-STR-BOUNDS throw then ;

: SUITE-LABEL! ( ptr u8 n -- ) {: src:ptr u:n :}
   u SUITE-NAME-CAP SUITE-CHECK-CAP
   src SUITE-LABEL-BUF u BYTE-COPY
   u SUITE-LABEL-U ! ;

: SUITE-LABEL$ ( -- ptr u8 n )
   SUITE-LABEL-BUF SUITE-LABEL-U @ ;

: SUITE-LABEL= ( ptr u8 n -- bool ) {: a:ptr u:n :}
   SUITE-LABEL$ a u STR= ;

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
   s" text-foundation-fixtures" SUITE-LABEL= if SUITE-TRUE exit then
   s" json-file-cursor" SUITE-LABEL= if SUITE-TRUE exit then
   s" imgdump-compare" SUITE-LABEL= if SUITE-TRUE exit then
   s" imagedisasm-tool" SUITE-LABEL= if SUITE-TRUE exit then
   s" streaming-sha256" SUITE-LABEL= if SUITE-TRUE exit then
   s" content-key-cache" SUITE-LABEL= if SUITE-TRUE exit then
   SUITE-FALSE ;

: SUITE-LINT-MANIFEST-LABEL? ( -- bool )
   s" stdlib-manifest" SUITE-LABEL= ;

: SUITE-LINT-LIBS-LABEL? ( -- bool )
   s" string-helpers" SUITE-LABEL= if SUITE-TRUE exit then
   s" array-helpers" SUITE-LABEL= if SUITE-TRUE exit then
   s" table-stdlib" SUITE-LABEL= if SUITE-TRUE exit then
   s" regex-stdlib" SUITE-LABEL= if SUITE-TRUE exit then
   s" map-stdlib" SUITE-LABEL= if SUITE-TRUE exit then
   s" ptx-stdlib" SUITE-LABEL= if SUITE-TRUE exit then
   s" ptx-tile-loop-neg" SUITE-LABEL= if SUITE-TRUE exit then
   s" ptx-tile-smem-neg" SUITE-LABEL= if SUITE-TRUE exit then
   s" ptx-tile-acc-neg" SUITE-LABEL= if SUITE-TRUE exit then
   s" ptx-gemm-checked-neg" SUITE-LABEL= if SUITE-TRUE exit then
   s" ptx-toolchain" SUITE-LABEL= if SUITE-TRUE exit then
   SUITE-FALSE ;

: SUITE-LINT? ( -- bool )
   SUITE-SLICE @ SUITE-LINT-ID = if
      SUITE-LINT-TOOLS-LABEL? if SUITE-TRUE exit then
      SUITE-LINT-MANIFEST-LABEL? if SUITE-TRUE exit then
      SUITE-LINT-ARTIFACTS-LABEL? if SUITE-TRUE exit then
      SUITE-LINT-LIBS-LABEL? exit
   then
   SUITE-SLICE @ SUITE-LINT-TOOLS-ID = if SUITE-LINT-TOOLS-LABEL? exit then
   SUITE-SLICE @ SUITE-LINT-MANIFEST-ID = if SUITE-LINT-MANIFEST-LABEL? exit then
   SUITE-SLICE @ SUITE-LINT-ARTIFACTS-ID = if SUITE-LINT-ARTIFACTS-LABEL? exit then
   SUITE-SLICE @ SUITE-LINT-LIBS-ID = if SUITE-LINT-LIBS-LABEL? exit then
   SUITE-FALSE ;

: SUITE-TOOL? ( -- bool )
   SUITE-SLICE @ SUITE-TOOL-ID <> if SUITE-FALSE exit then
   s" tool-boundary-trust" SUITE-LABEL= if SUITE-TRUE exit then
   s" tool-boundary-check-repair" SUITE-LABEL= if SUITE-TRUE exit then
   s" tool-boundary-doc-public" SUITE-LABEL= if SUITE-TRUE exit then
   s" tool-boundary-lints" SUITE-LABEL= if
      SUITE-SKIP-TOOL-LINTS @ 0= if SUITE-TRUE exit then
      SUITE-FALSE exit
   then
   s" tool-boundary-typed-local" SUITE-LABEL= if SUITE-TRUE exit then
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

: SUITE-STDIN! ( ptr u8 n -- ) {: src:ptr u:n :}
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

: SUITE-HB$ ( -- ptr u8 n )
   s" HABU_UNDER_TEST" GETENV dup 0= if
      2drop s" bin/hb" exit
   then
   2dup EXECUTABLE? 0= if E-FS-OPEN throw then ;

: SUITE-HB-RUN ( ptr u8 n -- ) {: label:ptr labelu:n :}
   s" inner-hb-spawn" GS-EVENT
   s" boundary-test" GS-EVENT
   SUITE-HB$ SUITE-TIMEOUT-MS label labelu SUITE-RUN-ENV-ASYNC ;

: SUITE-HB-RUN-STDIN ( ptr u8 n ptr u8 n -- ) {: in:ptr inu:n label:ptr labelu:n :}
   GT-POOL-DRAIN
   label labelu GT-PROGRESS-RUN
   s" inner-hb-stdin" GS-EVENT
   s" boundary-test" GS-EVENT
   SUITE-HB$ in inu SUITE-TIMEOUT-MS label labelu SUITE-RUN-STDIN
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
   s" tools/check-all-errors-core.f" SUITE-ARG+
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
