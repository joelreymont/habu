\ run-files.f - file sets that key native test suite caches.

require lib/errors.f
require lib/string.f

: TR-FILES-END? ( ptr u8 n -- bool )
   s" ;TR-FILES" STR= ;

: TR-FILES-ITEM, ( ptr u8 n -- ) {: a:ptr u:n :}
   u 0 < if E-STR-BOUNDS throw then
   u STR-BYTE-MAX > if E-STR-BOUNDS throw then
   u c,
   0 begin dup u < while
      dup a + c@ c,
      1+
   repeat drop ;

: TR-FILES-PARSE ( -- )
   begin
      parse-name dup 0= if 2drop E-STR-BOUNDS throw then
      2dup TR-FILES-END? if 2drop 0 c, exit then
      TR-FILES-ITEM,
   again ;

\ typed-local-lint: allow-bare-local - q keeps the quotation effect from the stack signature.
: TR-FILES-WALK ( ptr a [ ptr u8 n -- ] -- ) {: p:ptr q :}
   p begin dup c@ 0= 0= while
      dup 1+ over c@ q execute
      dup c@ 1 + +
   repeat drop ;

: TR-FILES-RUN ( [ ptr u8 n -- ] ptr a -- )
   swap TR-FILES-WALK ;

: TR-FILES: ( -- )
   create TR-FILES-PARSE
   does> ( [ ptr u8 n -- ] -- )
      TR-FILES-RUN ;

TR-FILES: TR-AOT-RUNNER-SUPPORT-FILES
   lib/errors.f lib/string.f lib/memory.f lib/vector.f lib/fs.f lib/fs-mutate.f
   lib/process.f lib/process-argv.f lib/process-env.f lib/process-fork.f lib/test/record.f lib/test/runner.f
   lib/source.f lib/build.f lib/codesign.f lib/content-key.f tools/build-fixpoint.f
   tools/stdin-closure-lib.f
   lib/object.f lib/object-cache.f lib/object-index.f lib/object-resolve.f
   lib/object-link.f tools/cli-run.f tools/object-image.f tools/hb-build-lib.f
   tools/lint/text.f tools/lint/token.f
   tools/lint/lib.f tools/lint/json-writer.f tools/lint/source-lex.f
   tools/aot-lint-core.f tools/signature-lint-core.f tools/hb-build-direct-lints.f
   tools/json.f tools/gate-json-assert-core.f tools/aot-call-report-lib.f
   test/gate-stats.f test/gate-common-lib.f test/gate-build-common.f
   test/gate-build-hbb.f src/habu/aot-closure.f
   test/gate-aot-positive-lib.f test/gate-aot-negative-lib.f
;TR-FILES

\ Result-cache phase file sets. A phase PASS-stamp key must cover every file
\ that can change the phase verdict; test/run-result-cache-test.f enforces
\ that each declared set is closed over require/include lines and existing
\ s" source literals (src/ members are keyed but not scanned). Phases with
\ no declared set are never result-cached. Date-sensitive phases (the
\ stale-status lint slice) must stay undeclared.

TR-FILES: TR-GATE-HARNESS-FILES
   test/run.f test/run-support.f test/run-lib.f test/run-files.f
   test/run-result-cache.f test/run-resident.f test/run-worker.f
   lib/errors.f lib/string.f lib/memory.f lib/vector.f lib/adt/option.f
   lib/adt/result.f lib/fs.f
   lib/fs-mutate.f lib/process.f lib/process-argv.f lib/process-env.f
   lib/process-fork.f lib/test/record.f lib/test/runner.f lib/test/budget.f
   lib/content-key.f tools/cli-run.f
   test/gate-pool.f test/gate-stats.f tools/why-threw.f
;TR-FILES

TR-FILES: TR-GATE-COMMON-FILES
   test/gate-common-lib.f tools/date.f lib/source.f tools/lint/text.f
   tools/lint/token.f tools/lint/lib.f tools/lint/json-writer.f
   tools/lint/source-lex.f tools/diag-origin-core.f tools/json.f
   tools/json-only-core.f tools/signature-lint-core.f
   tools/checked-boundary-lint-core.f tools/reserved-name-lint-core.f
   tools/trust-lint-core.f tools/check-all-errors-core.f tools/argv.f
   tools/dynamic-tail-manifest.f tools/source-discovery.f
   tools/check-core.f tools/check-main.f src/habu/verify-source.f
;TR-FILES

TR-FILES: TR-DEBUG-PHASE-FILES
   test/run-worker-debug.f test/gate-debug-lib.f tools/jitdump-core.f
   src/arch/arm64/disasm.f test/prop-test.f test/prop-test-core.f
;TR-FILES

TR-FILES: TR-AOT-NEG-PHASE-FILES
   test/run-worker-aot-neg.f test/gate-aot-negative-lib.f
   src/habu/aot-closure.f tools/gate-json-assert-core.f
;TR-FILES

TR-FILES: TR-UNDER-SOURCE-FILES
   tools/build-fixpoint.f tools/stdin-closure-lib.f src/habu/hide.f src/core/util.f
   src/core/structures.f src/core/checker.f
   src/core/lower-cert-base.f
   src/core/type-schema.f src/core/type-family.f
   src/core/render.f src/core/sumtype.f
   src/core/layout-buffer.f src/core/layout-valid.f
   src/core/check-hook.f src/core/structures-effects.f
   src/core/roles.f src/core/bytes.f
   src/arch/arm64/asm.f src/arch/arm64/icode.f src/arch/arm64/mnem.f
   src/habu/layout.f src/os/env-base.f src/os/script-argv.f
   src/core/enums.f src/core/exec-vector.f
   src/core/sha256.f src/core/type-family-sha.f
   src/core/combinators.f src/habu/treeshake.f
   src/habu/rt.f src/habu/crash.f src/os/image-bytes.f src/habu/habu1.f
   src/habu/prof.f src/habu/regalloc.f src/habu/jit.f src/habu/habu2.f
   src/habu/xref.f src/core/layout-buffer-seal.f src/core/lower-cert-seal.f
   src/habu/driver-io.f src/core/include.f src/habu/aot-capture.f
   src/habu/stage2.f src/habu/stdin.f src/habu/snap-lib.f src/habu/snap.f src/habu/repl.f
   src/habu/debug-watch.f src/habu/stepper.f src/habu/debug.f
   test/owner-wid-emitter.f test/owner-wid-image.f
   test/owner-wid-internal.f test/owner-wid-state.f test/owner-wid-call.f
;TR-FILES
