STDLIB-GATE:MAIN

using TEST

SUITE shadow-lint
   tools/lint/shadow-lint.f
   tools/lint/shadow-lint-test.f
;SUITE

SUITE clobber-lint
   tools/lint/clobber-lint.f
;SUITE

SUITE clobber-lint-fixtures
   tools/lint/clobber-lint-test.f
;SUITE

SUITE repl-lint
   tools/repl-lint.f
;SUITE

SUITE trust-lint
   tools/trust-lint.f
;SUITE

SUITE stale-status-lint
   tools/stale-status-lint.f
;SUITE

SUITE host-lint
   tools/host-lint.f
;SUITE

SUITE parallel-agent-lint
   tools/parallel-agent-lint.f
;SUITE

SUITE filemap-lint
   tools/filemap-lint.f
   tools/filemap-lint-test.f
;SUITE

SUITE ptx-emitter-lint
   tools/lint/ptx-emitter-lint.f
   tools/lint/ptx-emitter-lint-test.f
;SUITE

SUITE stdin-closure-lint
   tools/stdin-closure-lint.f
;SUITE

SUITE gate-stats
   test/gate-stats-test.f
;SUITE

SUITE dot-dep-lint
   tools/dot-dep-lint.f
;SUITE

SUITE dot-dep-lint-fixtures
   tools/dot-dep-lint-test.f
;SUITE

SUITE nanogpt-inventory-lint
   tools/nanogpt-inventory-lint.f
;SUITE

SUITE nanogpt-inventory-lint-fixtures
   tools/nanogpt-inventory-lint-test.f
;SUITE

SUITE maki-dep-lint
   tools/maki-dep-lint.f
;SUITE

SUITE maki-dep-lint-fixtures
   tools/maki-dep-lint-test.f
;SUITE

SUITE refine-lint
   tools/refine-lint.f
;SUITE

SUITE refine-lint-fixtures
   tools/refine-lint-test.f
;SUITE

SUITE suite-coverage-lint
   tools/suite-coverage-lint.f
   tools/suite-coverage-lint-test.f
;SUITE

SUITE namespace-lint
   tools/namespace-lint.f
;SUITE

SUITE namespace-lint-fixtures
   tools/namespace-lint-test.f
;SUITE

SUITE package-diff-lint-fixtures
   tools/package-diff-lint-test.f
;SUITE

SUITE error-code-lint
   tools/error-code-lint.f
;SUITE

SUITE error-code-lint-fixtures
   tools/error-code-lint-test.f
;SUITE

SUITE text-foundation-fixtures
   tools/lint/text-foundation-test.f
;SUITE

SUITE lint-intern-set
   tools/lint/set-test.f
;SUITE

SUITE diff-parser
   tools/lint/diff-test.f
;SUITE

SUITE diff-frame-codec
   tools/lint/diff-frame-test.f
;SUITE

SUITE stdlib-manifest
   tools/stdlib-manifest-test.f
;SUITE

SUITE host-lint-fixtures
   tools/host-lint-test.f
;SUITE

SUITE trusted-inventory
   tools/trusted-inventory-test.f
;SUITE

\ The census tool's own scanner fixtures. The production run over the real tree
\ (tools/enum-census.f verify) is registered separately as `enum-census`.
SUITE enum-census-fixtures
   tools/enum-census-test.f
;SUITE

\ Every plain ENUM declaration in the repository, re-declared through the global
\ ENUM keyword and compared against the baseline recorded before that keyword
\ moved to the unified front end.
SUITE enum-census
   tools/enum-census.f
;SUITE

SUITE primitive-effect-inventory
   tools/primitive-effect-inventory-test.f
;SUITE

SUITE json-file-cursor
   tools/json-file-test.f
;SUITE

SUITE imgdump-compare
   tools/imgdump-test.f
;SUITE

SUITE imagedisasm-tool
   tools/imagedisasm-test.f
;SUITE

SUITE tool-boundary-trust
   tools/trust-lint-test.f
   tools/aot-call-report-test.f
;SUITE

SUITE tool-boundary-check-repair
   tools/check-all-errors-test.f
   tools/repair-packet-test.f
;SUITE

SUITE tool-boundary-doc-public
   tools/public-signatures-test.f
   tools/stale-status-lint-test.f
   tools/repair-schema-doc-test.f
   tools/examples-test.f
;SUITE

SUITE tool-boundary-lints
   tools/repl-lint-test.f
   tools/diag-origin-test.f
   tools/aot-lint-test.f
   tools/signature-lint-test.f
   tools/checked-boundary-lint-test.f
   tools/reserved-name-lint-test.f
   tools/duplicate-definition-lint-test.f
   tools/bundle-lib-test.f
   tools/json-only-test.f
;SUITE

SUITE tool-boundary-typed-local
   tools/typed-local-diff-lint-test.f
;SUITE

SUITE check-cli-boundary
   tools/check-test.f
;SUITE

SUITE streaming-sha256
   tools/sha256-file-test.f
;SUITE

SUITE content-key-cache
   lib/content-key-test.f
;SUITE

SUITE engine-identity
   lib/engine-id-test.f
;SUITE

SUITE object-record-codec
   lib/object-test.f
;SUITE

SUITE object-cache-store
   lib/object-cache-test.f
;SUITE

SUITE object-source-index
   lib/object-index-test.f
;SUITE

SUITE object-source-resolver
   lib/object-resolve-test.f
;SUITE

SUITE object-link-symbols
   lib/object-link-test.f
;SUITE

SUITE object-image-writer
   tools/object-image-test.f
;SUITE

SUITE tasking-primitive-smoke
   test/atomics-smoke.f
   test/run-in-stack-smoke.f
;SUITE

SUITE getpid-primitive-smoke
   test/getpid-smoke.f
;SUITE

SUITE proc-watch-primitive-smoke
   test/proc-watch-smoke.f
;SUITE

SUITE proc-signal-primitive-smoke
   test/proc-signal-smoke.f
;SUITE

SUITE process-fork-wrappers
   lib/process-fork-test.f
;SUITE

SUITE proc-pty-io-supervisor-smoke
   test/process-pty-io-smoke.f
;SUITE

SUITE engine-candidate-resolver
   test/engine-candidate-test.f
;SUITE

SUITE tasking-threads
   lib/task-test.f
;SUITE

SUITE string-helpers
   lib/string-test.f
;SUITE

SUITE utf8-scalar
   lib/utf8-scalar-test.f
;SUITE

SUITE ffi-abi
   lib/ffi-abi-test.f
;SUITE

SUITE ffi-cabi
   lib/ffi-test.f
;SUITE

SUITE float-parse
   lib/float-test.f
   lib/fmath-test.f
;SUITE

SUITE ieee-float32
   lib/ieee754-test.f
   lib/float32-test.f
;SUITE

SUITE fmt-numbers
   lib/fmt-test.f
;SUITE

SUITE float-sort
   lib/sort-test.f
;SUITE

SUITE float-stats
   lib/stats-test.f
;SUITE

SUITE hashmap
   lib/hashmap-test.f
;SUITE

SUITE prelude
   lib/prelude-test.f
;SUITE

SUITE array-helpers
   lib/array-test.f
;SUITE

SUITE adt-result
   lib/adt/result-test.f
;SUITE

SUITE table-stdlib
   lib/table-test.f
;SUITE

SUITE regex-stdlib
   lib/regex-test.f
;SUITE

SUITE map-stdlib
   lib/map-test.f
;SUITE

SUITE codegen-stdlib
   lib/codegen-test.f
;SUITE

SUITE unicode-class-runtime
   lib/unicode/class-test.f
;SUITE

SUITE unicode-class-tools
   tools/unicode/class-tool-test.f
;SUITE

SUITE unicode-class-exhaustive
   tools/unicode/class-verify-main.f
;SUITE

SUITE ptx-stdlib
   lib/ptx/header-test.f
   lib/ptx/kernel-abi-test.f
   lib/ptx/kernel-manifest-test.f
   lib/ptx/launch-test.f
   lib/ptx/rep-test.f
   lib/ptx/mint-test.f
   lib/ptx/tile-test.f
   lib/ptx/tile-loop-test.f
   lib/ptx/tile-smem-test.f
   lib/ptx/tile-acc-test.f
   lib/ptx/gemm-checked-test.f
   lib/ptx/attention-checked-test.f
   lib/ptx/attention-roles-test.f
   lib/ptx/tile-v4-test.f
   lib/ptx/tile-v4a-test.f
   lib/ptx/tile-pipe-test.f
   lib/ptx/cpp-pipe-step-test.f
   lib/ptx/cpp-slot-test.f
   lib/ptx/collective-test.f
   lib/ptx/cg-collective-test.f
   lib/ptx/autograd-test.f
   lib/ptx/ir-test.f
   lib/ptx/opt-ir-test.f
   lib/ptx/opt-test.f
   lib/ptx/ad-test.f
   lib/ptx/ad-dag-test.f
   lib/ptx/ad-dag-eval-test.f
   lib/ptx/ad-saved-test.f
   lib/ptx/sentinel-test.f
   lib/ptx/cuda-driver-test.f
   lib/ptx/cuda-scope-test.f
   lib/ptx/ad-gen-test.f
   src/arch/ptx/vjp-test.f
;SUITE

SUITE ptx-rep-neg
   lib/ptx/rep-neg-test.f
;SUITE

SUITE ptx-mint-neg
   lib/ptx/mint-neg-test.f
;SUITE

SUITE ptx-tile-loop-neg
   lib/ptx/tile-loop-neg-test.f
;SUITE

SUITE ptx-tile-smem-neg
   lib/ptx/tile-smem-neg-test.f
;SUITE

SUITE ptx-tile-acc-neg
   lib/ptx/tile-acc-neg-test.f
;SUITE

SUITE ptx-tile-v4a-neg
   lib/ptx/tile-v4a-neg-test.f
;SUITE

SUITE ptx-tile-pipe-neg
   lib/ptx/tile-pipe-neg-test.f
;SUITE

SUITE ptx-cpp-slot-neg
   lib/ptx/cpp-slot-neg-test.f
   lib/ptx/cg-mma-slot-neg-test.f
;SUITE

SUITE ptx-gemm-checked-neg
   lib/ptx/gemm-checked-neg-test.f
;SUITE

SUITE ptx-attention-checked-neg
   lib/ptx/attention-checked-neg-test.f
;SUITE

SUITE ptx-autograd-neg
   lib/ptx/autograd-neg-test.f
;SUITE

SUITE ptx-uniform-barrier
   lib/ptx/uniform-barrier-test.f
;SUITE

SUITE ptx-toolchain
   lib/ptx/toolchain-test.f
   tools/ptx/profile-test.f
   tools/ptx/bench-test.f
   tools/ptx/saxpy-test.f
   tools/ptx/kernel-export-test.f
   tools/ptx/perf-registry-test.f
   tools/ptx/autotune-test.f
   tools/ptx/perf-compare-test.f
   tools/ptx/perf-regress-test.f
   tools/ptx/perf-regress.f
   tools/kernel-perf-lint-test.f
   tools/ptx/bandwidth-lib-test.f
   tools/ptx/mma-exact-lib-test.f
   tools/ptx/autotune-sweep-test.f
   tools/ptx/fusion-emit-test.f
   tools/ptx/device-gold-test.f
   tools/ptx/cuda-scope-leak-proof-test.f
   tools/ptx/attention-bench-test.f
   tools/ptx/fusion-compare.f
   tools/ptx/gemm-bench.f
   tools/ptx/attention-bench.f
   tools/ptx/acc-device-test.f
   tools/ptx/redadd-device-test.f
   tools/ptx/saxpy-v4-tail-device-test.f
   tools/ptx/device-gold.f
   tools/ptx/sum-launch.f
   tools/ptx/softmax-launch.f
   tools/ptx/softmax-gradcheck.f
   tools/ptx/rmsnorm-device-test.f
   tools/ptx/rope-device-test.f
   tools/ptx/layernorm-device-test.f
   tools/ptx/swiglu-device-test.f
   tools/ptx/cuda-launch.f
;SUITE

SUITE-STDIN source-stdlib-stdin DATA
   lib/source-test.f -- stdin
;SUITE

SUITE argv-stdlib-mocks
   lib/argv-test.f
;SUITE

SUITE argv-stdlib-script-args
   lib/argv-test.f -- --json --label NAME --strict-signatures --all-errors
   --strict-boundary -o OUT -- file.f --literal
;SUITE

SUITE test-stdlib
   lib/test/assert-test.f
   lib/test/suite-test.f
   lib/test/snap-test.f
   lib/test/record-test.f
   lib/test/src-shape-test.f
;SUITE

SUITE property-stdlib
   lib/property-test.f
;SUITE

SUITE date-helpers
   tools/stdlib-date-test.f
;SUITE

SUITE spawn-emitter-shape
   tools/spawn-emitter-test.f
;SUITE

SUITE c-call-emitter-shape
   tools/c-call-emitter-test.f
;SUITE

SUITE signature-scan-emitter-shape
   tools/signature-scan-emitter-test.f
;SUITE

SUITE compiler-dispatch-shape
   tools/compiler-dispatch-test.f
;SUITE

SUITE codegen-role
   tools/codegen-role-test.f
;SUITE

SUITE icode-fixup
   test/icode-fixup-test.f
;SUITE

SUITE engine-size
   test/engine-size-test.f
;SUITE

SUITE tail-pure-fixtures
   lib/json-write-test.f
   lib/json-read-test.f
   lib/json-read-perf-contract-test.f
   lib/memory-test.f
   lib/vector-test.f
   lib/byte-buffer-test.f
   lib/layout/box-test.f
   lib/fs-test.f
   tools/bootstrap-codegen-test.f
   tools/asm-src-test.f
   tools/asm-checked-test.f
   tools/image-bytes-test.f
;SUITE

SUITE stdlib-source-default
   lib/source-test.f
;SUITE

SUITE stdlib-process-fixtures
   tools/hb-cli-contracts-test.f
   tools/standalone-load-test.f
   lib/process-test.f
   lib/process-command-test.f
   lib/process-pty-handle-test.f
;SUITE

SUITE gate-environment-empty-stdin
   test/gate-env-stdin-tty-test.f
;SUITE

SUITE friend-arena-seal
   test/seal.f
;SUITE

SUITE internal-word-gate
   test/internal-word-gate.f
;SUITE

SUITE immediate-model
   test/immediate-model-test.f
;SUITE

SUITE pointer-storage
   test/pointer-storage-test.f
;SUITE

SUITE typed-storage
   test/typed-storage-test.f
;SUITE

SUITE underdepth-gate
   test/underdepth-gate.f
;SUITE

SUITE top-row-hook
   test/top-row-hook-test.f
;SUITE

SUITE top-row-warn
   test/top-row-warn-test.f
;SUITE

SUITE xt-effect
   test/xt-effect-test.f
;SUITE

SUITE xt-cell
   test/xt-cell-test.f
;SUITE

SUITE effect-read-api
   test/effect-read-api-test.f
;SUITE

SUITE checker-assert
   test/checker-assert-test.f
;SUITE

SUITE prim-link
   test/prim-link-test.f
;SUITE

SUITE verify-prim
   test/verify-prim-test.f
;SUITE

SUITE owner-wid-internal
   test/owner-wid-internal.f
;SUITE

SUITE owner-wid-snapshot
   test/owner-wid-snapshot.f
;SUITE

SUITE stdlib-standalone-load
   test/stdlib-standalone-load.f
;SUITE

SUITE aot-wid-restore
   test/aot-wid-suite.f
;SUITE

SUITE friend-arena-absence
   test/seal-absence.f
;SUITE

SUITE sealed-system-package
   test/seal-package.f
;SUITE

SUITE engine-error-package
   test/engine-error-package.f
;SUITE

SUITE pre-trust-defer
   test/pre-trust-defer.f
;SUITE

SUITE catch-frame
   test/catch-frame.f
;SUITE

SUITE export-keyword-package
   test/export-package.f
;SUITE

SUITE gate-runner-entry-load
   test/gate-runner-entry-test.f
;SUITE

SUITE load-reject-diag
   test/load-reject-diag-test.f
;SUITE

SUITE dictionary-record-shapes
   test/drec-shape-test.f
;SUITE

SUITE stdlib-runner-fixtures
   lib/test/runner-test.f
;SUITE

SUITE stdlib-build-fixtures
   lib/build-test.f
;SUITE

SUITE build-fixpoint-fixtures
   tools/build-fixpoint-test.f
;SUITE

SUITE boot-pin-fixtures
   test/boot-pin-test.f
;SUITE

SUITE hb-build-fixtures
   tools/hb-build-test.f
   lib/build-cache-test.f
   lib/codesign-test.f
   tools/hb-build-direct-lints-test.f
;SUITE

SUITE gate-pool
   test/gate-pool-test.f
   test/json-read-perf-phase-test.f
;SUITE

package STDLIB-GATE public get-current ;package

package STDLIB-GATE-TEST

constant TARGET-WID

: REQUIRE-FOUND ( n -- )
   0= if E-TBL-BOUNDS throw then ;

: REQUIRE-MISSING ( n -- )
   0= 0= if E-TBL-BOUNDS throw then ;

: RUN ( -- )
   s" MAIN" TARGET-WID search-wl REQUIRE-FOUND
   s" SKIP-SEMANTIC!" TARGET-WID search-wl REQUIRE-FOUND
   s" SUITE-CHECK-CLI?" TARGET-WID search-wl REQUIRE-MISSING
   s" GATE-STDLIB-MAIN" 0 search-wl REQUIRE-MISSING
   s" SUITE-SKIP-TOOL-SEMANTIC!" 0 search-wl REQUIRE-MISSING ;

: ACTION ( -- [ -- ] )
   [: RUN ;] ;

ACTION

;package

execute

RUN

;using

s" PASS: native lint/stdlib test phase" type cr
