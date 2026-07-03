GATE-STDLIB-MAIN

TEST:SUITE shadow-lint
   tools/lint/shadow-lint.f
TEST:END-SUITE

TEST:SUITE clobber-lint
   tools/lint/clobber-lint.f
TEST:END-SUITE

TEST:SUITE clobber-lint-fixtures
   tools/lint/clobber-lint-test.f
TEST:END-SUITE

TEST:SUITE repl-lint
   tools/repl-lint.f
TEST:END-SUITE

TEST:SUITE trust-lint
   tools/trust-lint.f
TEST:END-SUITE

TEST:SUITE stale-status-lint
   tools/stale-status-lint.f
TEST:END-SUITE

TEST:SUITE host-lint
   tools/host-lint.f
TEST:END-SUITE

TEST:SUITE parallel-agent-lint
   tools/parallel-agent-lint.f
TEST:END-SUITE

TEST:SUITE filemap-lint
   tools/filemap-lint.f
   tools/filemap-lint-test.f
TEST:END-SUITE

TEST:SUITE gate-stats
   test/gate-stats-test.f
TEST:END-SUITE

TEST:SUITE dot-dep-lint
   tools/dot-dep-lint.f
TEST:END-SUITE

TEST:SUITE dot-dep-lint-fixtures
   tools/dot-dep-lint-test.f
TEST:END-SUITE

TEST:SUITE maki-dep-lint
   tools/maki-dep-lint.f
TEST:END-SUITE

TEST:SUITE maki-dep-lint-fixtures
   tools/maki-dep-lint-test.f
TEST:END-SUITE

TEST:SUITE text-foundation-fixtures
   tools/lint/text-foundation-test.f
TEST:END-SUITE

TEST:SUITE stdlib-manifest
   tools/stdlib-manifest-test.f
TEST:END-SUITE

TEST:SUITE host-lint-fixtures
   tools/host-lint-test.f
TEST:END-SUITE

TEST:SUITE json-file-cursor
   tools/json-file-test.f
TEST:END-SUITE

TEST:SUITE imgdump-compare
   tools/imgdump-test.f
TEST:END-SUITE

TEST:SUITE imagedisasm-tool
   tools/imagedisasm-test.f
TEST:END-SUITE

TEST:SUITE tool-boundary-trust
   tools/trust-lint-test.f
   tools/aot-call-report-test.f
TEST:END-SUITE

TEST:SUITE tool-boundary-check-repair
   tools/check-all-errors-test.f
   tools/repair-packet-test.f
TEST:END-SUITE

TEST:SUITE tool-boundary-doc-public
   tools/public-signatures-test.f
   tools/stale-status-lint-test.f
   tools/repair-schema-doc-test.f
   tools/examples-test.f
TEST:END-SUITE

TEST:SUITE tool-boundary-lints
   tools/repl-lint-test.f
   tools/diag-origin-test.f
   tools/aot-lint-test.f
   tools/signature-lint-test.f
   tools/checked-boundary-lint-test.f
   tools/reserved-name-lint-test.f
   tools/duplicate-definition-lint-test.f
   tools/bundle-lib-test.f
   tools/json-only-test.f
TEST:END-SUITE

TEST:SUITE tool-boundary-typed-local
   tools/typed-local-diff-lint-test.f
TEST:END-SUITE

TEST:SUITE check-cli-boundary
   tools/check-test.f
TEST:END-SUITE

TEST:SUITE streaming-sha256
   tools/sha256-file-test.f
TEST:END-SUITE

TEST:SUITE content-key-cache
   lib/content-key-test.f
TEST:END-SUITE

TEST:SUITE object-record-codec
   lib/object-test.f
TEST:END-SUITE

TEST:SUITE object-cache-store
   lib/object-cache-test.f
TEST:END-SUITE

TEST:SUITE object-source-index
   lib/object-index-test.f
TEST:END-SUITE

TEST:SUITE object-source-resolver
   lib/object-resolve-test.f
TEST:END-SUITE

TEST:SUITE object-link-symbols
   lib/object-link-test.f
TEST:END-SUITE

TEST:SUITE object-image-writer
   tools/object-image-test.f
TEST:END-SUITE

TEST:SUITE tasking-primitive-smoke
   test/atomics-smoke.f
   test/run-in-stack-smoke.f
TEST:END-SUITE

TEST:SUITE tasking-threads
   lib/task-test.f
TEST:END-SUITE

TEST:SUITE string-helpers
   lib/string-test.f
TEST:END-SUITE

TEST:SUITE ffi-abi
   lib/ffi-abi-test.f
TEST:END-SUITE

TEST:SUITE ffi-cabi
   lib/ffi-test.f
TEST:END-SUITE

TEST:SUITE float-parse
   lib/float-test.f
TEST:END-SUITE

TEST:SUITE fmt-numbers
   lib/fmt-test.f
TEST:END-SUITE

TEST:SUITE float-sort
   lib/sort-test.f
TEST:END-SUITE

TEST:SUITE float-stats
   lib/stats-test.f
TEST:END-SUITE

TEST:SUITE hashmap
   lib/hashmap-test.f
TEST:END-SUITE

TEST:SUITE prelude
   lib/prelude-test.f
TEST:END-SUITE

TEST:SUITE array-helpers
   lib/array-test.f
TEST:END-SUITE

TEST:SUITE table-stdlib
   lib/table-test.f
TEST:END-SUITE

TEST:SUITE regex-stdlib
   lib/regex-test.f
TEST:END-SUITE

TEST:SUITE map-stdlib
   lib/map-test.f
TEST:END-SUITE

TEST:SUITE ptx-stdlib
   lib/ptx/header-test.f
   lib/ptx/launch-test.f
   lib/ptx/tile-test.f
   lib/ptx/tile-loop-test.f
   lib/ptx/tile-smem-test.f
   lib/ptx/tile-acc-test.f
   lib/ptx/gemm-checked-test.f
   lib/ptx/tile-v4-test.f
   lib/ptx/collective-test.f
   lib/ptx/autograd-test.f
   lib/ptx/ir-test.f
   lib/ptx/ad-test.f
   lib/ptx/ad-dag-test.f
   lib/ptx/ad-saved-test.f
TEST:END-SUITE

TEST:SUITE ptx-tile-loop-neg
   lib/ptx/tile-loop-neg-test.f
TEST:END-SUITE

TEST:SUITE ptx-tile-smem-neg
   lib/ptx/tile-smem-neg-test.f
TEST:END-SUITE

TEST:SUITE ptx-tile-acc-neg
   lib/ptx/tile-acc-neg-test.f
TEST:END-SUITE

TEST:SUITE ptx-gemm-checked-neg
   lib/ptx/gemm-checked-neg-test.f
TEST:END-SUITE

TEST:SUITE ptx-toolchain
   lib/ptx/toolchain-test.f
   tools/ptx/profile-test.f
   tools/ptx/bench-test.f
   tools/ptx/saxpy-test.f
TEST:END-SUITE

TEST:SUITE-STDIN source-stdlib-stdin DATA
   lib/source-test.f -- stdin
TEST:END-SUITE

TEST:SUITE argv-stdlib-mocks
   lib/argv-test.f
TEST:END-SUITE

TEST:SUITE argv-stdlib-script-args
   lib/argv-test.f -- --json --label NAME --strict-signatures --all-errors
   --strict-boundary -o OUT -- file.f --literal
TEST:END-SUITE

TEST:SUITE test-stdlib
   lib/test/assert-test.f
   lib/test/suite-test.f
   lib/test/snap-test.f
   lib/test/record-test.f
TEST:END-SUITE

TEST:SUITE property-stdlib
   lib/property-test.f
TEST:END-SUITE

TEST:SUITE date-helpers
   tools/date-test.f
TEST:END-SUITE

TEST:SUITE spawn-emitter-shape
   tools/spawn-emitter-test.f
TEST:END-SUITE

TEST:SUITE c-call-emitter-shape
   tools/c-call-emitter-test.f
TEST:END-SUITE

TEST:SUITE signature-scan-emitter-shape
   tools/signature-scan-emitter-test.f
TEST:END-SUITE

TEST:SUITE compiler-dispatch-shape
   tools/compiler-dispatch-test.f
TEST:END-SUITE

TEST:SUITE tail-pure-fixtures
   lib/json-write-test.f
   lib/memory-test.f
   lib/vector-test.f
   lib/fs-test.f
   tools/bootstrap-codegen-test.f
   tools/asm-src-test.f
   tools/asm-checked-test.f
   tools/image-bytes-test.f
TEST:END-SUITE

TEST:SUITE stdlib-source-default
   lib/source-test.f
TEST:END-SUITE

TEST:SUITE stdlib-process-fixtures
   tools/hb-cli-contracts-test.f
   lib/process-test.f
   lib/process-command-test.f
TEST:END-SUITE

TEST:SUITE stdlib-runner-fixtures
   lib/test/runner-test.f
TEST:END-SUITE

TEST:SUITE stdlib-build-fixtures
   lib/build-test.f
TEST:END-SUITE

TEST:SUITE build-fixpoint-fixtures
   tools/build-fixpoint-test.f
TEST:END-SUITE

TEST:SUITE hb-build-fixtures
   tools/hb-build-test.f
   lib/codesign-test.f
TEST:END-SUITE

TEST:SUITE gate-pool
   test/gate-pool-test.f
TEST:END-SUITE

TEST:RUN
s" PASS: native lint/stdlib test phase" type cr
