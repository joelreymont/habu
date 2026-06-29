GATE-STDLIB-MAIN

TEST-SUITE shadow-lint
   lib/errors.f lib/string.f tools/lint/text.f tools/lint/token.f tools/lint/lib.f
   tools/lint/shadow-lint.f
;TEST-SUITE

TEST-SUITE clobber-lint
   lib/errors.f lib/string.f tools/lint/text.f tools/lint/token.f tools/lint/lib.f
   tools/lint/clobber-lint.f
;TEST-SUITE

TEST-SUITE clobber-lint-fixtures
   lib/errors.f lib/string.f tools/lint/text.f tools/lint/token.f tools/lint/lib.f
   tools/lint/clobber-lint.f tools/lint/clobber-lint-test.f
;TEST-SUITE

TEST-SUITE repl-lint
   lib/errors.f lib/string.f lib/memory.f lib/vector.f tools/lint/text.f
   tools/lint/intern.f tools/lint/token.f tools/lint/lib.f
   tools/repl-lint-core.f tools/argv.f tools/repl-lint.f
;TEST-SUITE

TEST-SUITE trust-lint
   tools/date.f lib/errors.f lib/string.f lib/memory.f lib/fs.f tools/lint/text.f
   tools/lint/token.f tools/lint/lib.f tools/trust-lint-core.f tools/argv.f
   tools/trust-lint.f
;TEST-SUITE

TEST-SUITE stale-status-lint
   tools/date.f lib/errors.f lib/string.f lib/fs.f tools/lint/text.f
   tools/lint/token.f tools/lint/lib.f tools/argv.f
   tools/stale-status-lint-core.f
   tools/stale-status-lint.f
;TEST-SUITE

TEST-SUITE host-lint
   lib/errors.f lib/string.f lib/fs.f tools/lint/text.f
   tools/lint/token.f tools/lint/lib.f tools/host-lint.f
;TEST-SUITE

TEST-SUITE parallel-agent-lint
   lib/errors.f lib/string.f tools/lint/text.f tools/lint/token.f tools/lint/lib.f
   tools/parallel-agent-lint.f
;TEST-SUITE

TEST-SUITE filemap-lint
   lib/errors.f lib/string.f lib/memory.f lib/vector.f tools/lint/text.f
   tools/lint/intern.f tools/lint/token.f tools/lint/lib.f
   tools/filemap-lint.f
;TEST-SUITE

TEST-SUITE gate-stats
   lib/errors.f lib/string.f lib/test.f lib/memory.f lib/fs.f lib/fs-mutate.f
   lib/process.f lib/process-argv.f lib/process-env.f
   test/gate-stats.f test/gate-stats-test.f
;TEST-SUITE

TEST-SUITE dot-dep-lint
   lib/errors.f lib/string.f lib/memory.f lib/vector.f lib/fs.f
   tools/lint/text.f tools/lint/intern.f tools/dot-dep-lint-core.f
   tools/dot-dep-lint.f
;TEST-SUITE

TEST-SUITE dot-dep-lint-fixtures
   lib/errors.f lib/string.f lib/test.f lib/memory.f lib/vector.f lib/fs.f
   tools/lint/text.f tools/lint/intern.f tools/dot-dep-lint-core.f
   tools/dot-dep-lint-test.f
;TEST-SUITE

TEST-SUITE maki-dep-lint
   lib/errors.f lib/string.f lib/memory.f lib/vector.f lib/fs.f
   tools/lint/text.f tools/lint/token.f tools/maki-dep-lint-core.f
   tools/maki-dep-lint.f
;TEST-SUITE

TEST-SUITE maki-dep-lint-fixtures
   lib/errors.f lib/string.f lib/test.f lib/memory.f lib/vector.f lib/fs.f
   tools/lint/text.f tools/lint/token.f tools/maki-dep-lint-core.f
   tools/maki-dep-lint-test.f
;TEST-SUITE

TEST-SUITE text-foundation-fixtures
   lib/errors.f lib/string.f lib/memory.f lib/vector.f tools/lint/text.f
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
   lib/errors.f lib/string.f lib/test.f lib/memory.f lib/fs.f lib/fs-mutate.f
   lib/process.f lib/process-argv.f tools/imgdump.f tools/imgdump-test.f
;TEST-SUITE

TEST-SUITE imagedisasm-tool
   lib/errors.f lib/string.f lib/test.f lib/memory.f lib/fs.f lib/fs-mutate.f
   lib/process.f lib/process-argv.f src/arch/arm64/disasm.f
   tools/imagedisasm.f tools/imagedisasm-test.f
;TEST-SUITE

TEST-TOOL-SUITE tool-boundary-trust
   tools/trust-lint-core.f tools/trust-lint-test.f
   tools/aot-call-report.f tools/aot-call-report-test.f
;TEST-SUITE

TEST-TOOL-SUITE tool-boundary-check-repair
   tools/check-all-errors-test.f tools/repair-packet-core.f
   tools/gate-json-assert-core.f
   tools/repair-packet-test.f
;TEST-SUITE

TEST-TOOL-SUITE tool-boundary-doc-public
   tools/public-signatures-core.f tools/public-signatures-test.f
   tools/stale-status-lint-core.f
   tools/stale-status-lint-test.f tools/gate-json-assert-core.f
   tools/repair-schema-doc-test.f
   tools/examples-test.f
;TEST-SUITE

TEST-TOOL-SUITE tool-boundary-lints
   tools/repl-lint-core.f tools/repl-lint-test.f tools/diag-origin-test.f
   tools/aot-lint-core.f tools/aot-lint-test.f
   tools/signature-lint-core.f tools/signature-lint-test.f
   tools/checked-boundary-lint-core.f tools/checked-boundary-lint-test.f
   tools/reserved-name-lint-core.f tools/reserved-name-lint-test.f
   tools/duplicate-definition-lint-core.f tools/duplicate-definition-lint-test.f
   tools/bundle-lib-core.f tools/bundle-lib-test.f tools/json-only-test.f
;TEST-SUITE

TEST-TOOL-SUITE tool-boundary-typed-local
   tools/typed-local-diff-lint-core.f tools/typed-local-diff-lint-test.f
;TEST-SUITE

TEST-SUITE check-cli-boundary
   tools/date.f lib/errors.f lib/string.f lib/test.f lib/memory.f lib/vector.f lib/fs.f lib/fs-mutate.f
   lib/process.f lib/process-argv.f lib/source.f tools/lint/text.f
   tools/lint/token.f tools/lint/lib.f tools/lint/json-writer.f
   tools/lint/source-lex.f tools/diag-origin-core.f tools/json.f
   tools/json-only-core.f tools/signature-lint-core.f
   tools/checked-boundary-lint-core.f tools/reserved-name-lint-core.f
   tools/typed-local-diff-lint-core.f
   tools/trust-lint-core.f
   tools/check-all-errors-core.f tools/argv.f tools/warm-run.f
   tools/check-core.f tools/check-test.f
;TEST-SUITE

TEST-SUITE streaming-sha256
   lib/errors.f lib/string.f lib/test.f lib/fs.f lib/fs-mutate.f
   tools/sha256-file-test.f
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

TEST-SUITE hashmap
   lib/errors.f lib/string.f lib/test.f lib/hashmap.f lib/hashmap-test.f
;TEST-SUITE

TEST-SUITE prelude
   lib/errors.f lib/string.f lib/test.f lib/float.f lib/prelude.f lib/prelude-test.f
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
   lib/errors.f lib/string.f lib/float.f lib/fmt.f lib/test.f src/arch/ptx/emit.f lib/ptx/cg.f lib/ptx/cg-vec.f lib/ptx/cg-collective.f lib/ptx/header.f lib/ptx/header-test.f lib/ptx/tile.f lib/ptx/tile-test.f lib/ptx/tile-loop.f lib/ptx/tile-loop-test.f lib/ptx/tile-smem.f lib/ptx/tile-smem-test.f lib/ptx/tile-acc.f lib/ptx/tile-acc-test.f lib/ptx/gemm-checked-test.f lib/ptx/tile-v4.f lib/ptx/tile-v4-test.f lib/ptx/collective.f lib/ptx/collective-test.f lib/ptx/autograd-test.f lib/ptx/ad.f lib/ptx/ad-test.f lib/ptx/ad-saved.f lib/ptx/ad-saved-test.f
;TEST-SUITE

TEST-SUITE ptx-tile-loop-neg
   lib/errors.f lib/string.f lib/test.f lib/memory.f lib/fs.f lib/process.f
   lib/process-argv.f lib/process-env.f lib/ptx/tile-loop-neg-test.f
;TEST-SUITE

TEST-SUITE ptx-tile-smem-neg
   lib/errors.f lib/string.f lib/test.f lib/memory.f lib/fs.f lib/process.f
   lib/process-argv.f lib/process-env.f lib/ptx/tile-smem-neg-test.f
;TEST-SUITE

TEST-SUITE ptx-tile-acc-neg
   lib/errors.f lib/string.f lib/test.f lib/memory.f lib/fs.f lib/process.f
   lib/process-argv.f lib/process-env.f lib/ptx/tile-acc-neg-test.f
;TEST-SUITE

TEST-SUITE ptx-gemm-checked-neg
   lib/errors.f lib/string.f lib/test.f lib/memory.f lib/fs.f lib/process.f
   lib/process-argv.f lib/process-env.f lib/ptx/gemm-checked-neg-test.f
;TEST-SUITE

TEST-SUITE ptx-toolchain
   lib/errors.f lib/string.f lib/test.f lib/memory.f lib/fs.f lib/process.f
   lib/process-argv.f lib/process-env.f src/arch/ptx/emit.f
   tools/ptx/saxpy-test.f
;TEST-SUITE

TEST-SUITE-STDIN source-stdlib-stdin DATA
   lib/errors.f lib/string.f lib/test.f lib/fs.f lib/fs-mutate.f
   lib/memory.f lib/source.f lib/source-test.f -- stdin
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
   tools/image-bytes-test.f
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
   lib/content-key.f tools/warm-run.f tools/hb-build-lib.f tools/hb-build-test.f
   lib/codesign-test.f
;TEST-SUITE

GT-POOL-DRAIN
SUITE-CLEANUP
s" PASS: native lint/stdlib gate phase" type cr
