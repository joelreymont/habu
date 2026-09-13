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

SUITE dot-dep-lint-fixtures
   tools/dot-dep-lint-test.f
;SUITE

SUITE error-code-lint-fixtures
   tools/error-code-lint-test.f
;SUITE

SUITE text-foundation-fixtures
   tools/lint/text-foundation-test.f
;SUITE

SUITE lint-def-fixtures
   tools/lint/def-test.f
;SUITE

SUITE lint-intern-set
   tools/lint/set-test.f
;SUITE

SUITE diff-parser
   tools/lint/diff-test.f
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

SUITE tool-boundary-aot-call
   tools/aot-call-report-test.f
;SUITE

SUITE tool-boundary-check-repair
   tools/check-all-errors-test.f
   tools/repair-packet-test.f
;SUITE

SUITE tool-boundary-doc-public
   tools/public-signatures-test.f
   tools/public-signatures-bracket-test.f
   tools/repair-schema-doc-test.f
   tools/examples-test.f
;SUITE

SUITE tool-boundary-lints
   tools/repl-lint-test.f
   tools/diag-origin-test.f
   tools/aot-lint-test.f
   tools/checked-boundary-lint-test.f
   tools/reserved-name-lint-test.f
   tools/bundle-lib-test.f
   tools/json-only-test.f
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

SUITE compiler-ir-id
   test/compiler/ir-id.f
;SUITE

SUITE compiler-ir-id-manifest
   test/compiler/ir-id-manifest.f
;SUITE

SUITE compiler-ir-intern-manifest
   test/compiler/ir-intern-manifest.f
;SUITE

\ Each proof manifest directly asserts its obligation rows and schema; the
\ corresponding proof test below runs Rocq.
SUITE compiler-ir-structure-manifest
   test/compiler/ir-structure-manifest.f
;SUITE

SUITE compiler-ir-storage-manifest
   test/compiler/ir-storage-manifest.f
;SUITE

SUITE compiler-checker-model-manifest
   test/compiler/checker-model-manifest.f
;SUITE

SUITE compiler-insn-manifest
   test/compiler/insn-manifest.f
;SUITE

SUITE compiler-reloc-manifest
   test/compiler/reloc-manifest.f
;SUITE

SUITE compiler-ir-schema
   test/compiler/ir-schema.f
;SUITE

SUITE compiler-ir-op
   test/compiler/ir-op.f
;SUITE

SUITE compiler-ir-fun
   test/compiler/ir-fun.f
;SUITE

SUITE compiler-ir-build
   test/compiler/ir-build.f
;SUITE

SUITE compiler-ir-verify
   test/compiler/ir-verify.f
;SUITE

SUITE compiler-ir-arena
   test/compiler/ir-arena.f
;SUITE

SUITE compiler-ir-attr
   test/compiler/ir-attr.f
;SUITE

SUITE compiler-ir-context
   test/compiler/ir-context.f
;SUITE

SUITE compiler-ir-source
   test/compiler/ir-source.f
;SUITE

SUITE compiler-ir-symbol
   test/compiler/ir-symbol.f
;SUITE

SUITE compiler-ir-type
   test/compiler/ir-type.f
;SUITE

SUITE compiler-native-tape
   test/compiler/native-tape.f
;SUITE

SUITE compiler-native-feed
   test/compiler/native-feed.f
;SUITE

SUITE compiler-native-tape-owner
   test/compiler/aot-mode.f
   test/compiler/native-tape-owner.f
;SUITE

SUITE compiler-native-string
   test/compiler/native-string.f
;SUITE

SUITE compiler-native-hir
   test/compiler/native-hir.f
;SUITE

SUITE compiler-native-elaborate
   test/compiler/native-elaborate.f
;SUITE

SUITE compiler-asm-package
   test/compiler/asm-package-test.f
;SUITE

SUITE compiler-native-a64ir
   test/compiler/native-a64ir.f
;SUITE

\ The typed ARM64 routine-effect schema, next to the a64 lowering it constrains.
\ It was fork-only, so the register bounds it pins were unchecked in a standalone
\ gate run.
SUITE compiler-a64-effect
   test/compiler/a64-effect.f
;SUITE

\ The target/policy binding: src/compiler/digest.f, target.f, numeric-policy.f
\ and binding.f through their public words. It is the acceptance suite
\ habu-bind-compiler-target-b3dfa307 is answered by, and a suite that answers a
\ dot has to be reachable by name in the registry, not only inside a fork list -
\ that missing row is what blocked the dot.
SUITE compiler-target-policy
   test/compiler/target-policy.f
;SUITE

SUITE compiler-native-select
   test/compiler/native-select.f
;SUITE

SUITE compiler-native-regalloc
   test/compiler/native-regalloc.f
;SUITE

SUITE compiler-native-address-spill
   test/compiler/native-address-spill.f
;SUITE

SUITE compiler-native-loop-frame-order
   test/compiler/native-loop-frame-order.f
;SUITE

SUITE compiler-native-arm-frame-order
   test/compiler/native-arm-frame-order.f
;SUITE

SUITE compiler-native-emit
   test/compiler/native-emit.f
;SUITE

SUITE compiler-native-session
   test/compiler/native-session.f
;SUITE

SUITE compiler-native-trap
   test/compiler/native-trap.f
;SUITE

SUITE compiler-native-quot
   test/compiler/native-quot.f
;SUITE

SUITE compiler-native-defer
   test/compiler/native-defer.f
;SUITE

SUITE compiler-native-layout-control
   test/compiler/native-layout-control.f
;SUITE

SUITE compiler-native-prefix-declarations
   test/compiler/native-prefix-declarations.f
;SUITE

SUITE primitive-trust
   test/primitive-trust.f
;SUITE

SUITE native-window-owner
   test/native-window-owner.f
;SUITE

SUITE loop-obligations
   test/loop-obligations.f
;SUITE

SUITE native-build-layout
   test/native-layout.f
;SUITE

SUITE native-build-entry
   test/native-build-entry.f
;SUITE

SUITE compiler-native-exec
   test/compiler/native-exec.f
;SUITE

SUITE compiler-native-generic-calls
   test/compiler/native-generic-calls.f
;SUITE

SUITE compiler-native-provider-rows
   test/compiler/native-provider-rows.f
;SUITE

SUITE compiler-native-internal-call
   test/compiler/native-internal-call.f
;SUITE

SUITE compiler-native-stored-quot
   test/compiler/native-stored-quot.f
;SUITE

SUITE image-lifecycle
   test/image-lifecycle.f
;SUITE

SUITE image-lifecycle-tasks
   test/image-lifecycle-tasks.f
;SUITE

SUITE native-resource-image
   test/native-resource-image.f
;SUITE

SUITE app-image
   test/app-image.f
;SUITE

SUITE registry-persist
   test/registry-persist.f
;SUITE

SUITE native-defer-image
   test/native-defer-image.f
;SUITE

SUITE process-image
   test/process-image.f
;SUITE

SUITE compiler-native-many-locals
   test/compiler/native-many-locals.f
;SUITE

SUITE compiler-native-tail-owner
   test/compiler/native-tail-owner.f
;SUITE

SUITE code-reclaim
   test/code-reclaim.f
;SUITE

SUITE compiler-codegen-tail-probe
   test/compiler/codegen-tail-probe.f
;SUITE

SUITE compiler-native-tail
   test/compiler/native-tail.f
;SUITE

SUITE compiler-native-combine
   test/compiler/native-combine.f
;SUITE

SUITE compiler-native-loop
   test/compiler/native-loop.f
;SUITE

SUITE compiler-native-edge-permutation
   test/compiler/native-edge-permutation.f
;SUITE

SUITE compiler-native-switch
   test/compiler/native-switch.f
;SUITE

SUITE compiler-native-do
   test/compiler/native-do.f
;SUITE

SUITE compiler-native-j
   test/compiler/native-j.f
;SUITE

SUITE compiler-native-again
   test/compiler/native-again.f
;SUITE

SUITE compiler-native-leave
   test/compiler/native-leave.f
;SUITE

SUITE compiler-native-rstack
   test/compiler/native-rstack.f
;SUITE

SUITE compiler-native-catch
   test/compiler/native-catch.f
;SUITE

SUITE compiler-native-declaration-diagnostic
   test/compiler/native-declaration-diagnostic.f
;SUITE

SUITE finally
   test/finally.f
;SUITE

SUITE compiler-native-locals-scope
   test/compiler/native-locals-scope.f
;SUITE

SUITE compiler-native-local-case
   test/compiler/native-local-case.f
;SUITE

SUITE compiler-native-product-locals
   test/compiler/native-product-locals.f
;SUITE

SUITE compiler-native-word-binding
   test/compiler/native-word-binding.f
;SUITE

SUITE compiler-native-dictionary-record
   test/compiler/native-dictionary-record.f
;SUITE

SUITE compiler-native-dictionary-append
   test/compiler/native-dictionary-append.f
;SUITE

SUITE compiler-native-dictionary-publish
   test/compiler/native-dictionary-publish.f
;SUITE

SUITE compiler-native-quot-scope
   test/compiler/native-quot-scope.f
;SUITE

SUITE compiler-native-chain
   test/compiler/native-chain.f
;SUITE

SUITE compiler-native-qualified-name
   test/compiler/native-qualified-name.f
;SUITE

SUITE compiler-native-generated-constructor
   test/compiler/native-generated-constructor.f
;SUITE

SUITE compiler-native-order-exit
   test/compiler/native-order-exit.f
;SUITE

SUITE compiler-native-exit
   test/compiler/native-exit.f
;SUITE

SUITE compiler-native-tick
   test/compiler/native-tick.f
;SUITE

SUITE compiler-native-literals
   test/compiler/native-literals.f
;SUITE

SUITE compiler-native-eval
   test/compiler/native-eval.f
;SUITE

SUITE native-dead-path
   test/compiler/native-dead-path.f
;SUITE

SUITE native-dstack-alias
   test/compiler/native-dstack-alias.f
;SUITE

SUITE native-tail-placement
   test/compiler/native-tail-placement.f
;SUITE

SUITE compiler-native-match
   test/compiler/native-match.f
;SUITE

SUITE compiler-native-rename-rows
   test/compiler/native-rename-rows.f
;SUITE

SUITE compiler-native-wide-mem
   test/compiler/native-wide-mem.f
;SUITE

SUITE compiler-native-fetch-terms
   test/compiler/native-fetch-terms.f
;SUITE

SUITE compiler-native-vocab
   test/compiler/native-vocab.f
;SUITE

\ The identity parity gate compiles formal/Common with the Rocq proof assistant
\ and spawns child engines through this registry entry.
SUITE compiler-ir-id-proof
   test/compiler/ir-id-proof.f
;SUITE

\ The interning parity test compiles formal/Common/Interning.v with Rocq.
SUITE compiler-ir-intern-proof
   test/compiler/ir-intern-proof.f
;SUITE

\ The structure parity test compiles formal/Common/Structure.v with Rocq.
SUITE compiler-ir-structure-proof
   test/compiler/ir-structure-proof.f
;SUITE

\ The storage and lifetime parity test compiles formal/Common/Storage.v with Rocq.
SUITE compiler-ir-storage-proof
   test/compiler/ir-storage-proof.f
;SUITE

\ The checker model parity test compiles formal/Common/Effects.v and
\ formal/Common/Control.v with Rocq.
SUITE checker-model-proof
   test/compiler/checker-model-proof.f
;SUITE

\ The snapshot relocation parity test compiles formal/Common/Reloc.v with Rocq.
SUITE compiler-reloc-proof
   test/compiler/reloc-proof.f
;SUITE

\ The instruction-encoding parity test drives Rocq and child engines.
SUITE compiler-insn-proof
   test/compiler/insn-proof.f
;SUITE

SUITE raw-storage-load-seal
   test/raw-storage-load-seal-test.f
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

SUITE proc-pty-tty-smoke
   test/process-pty-tty-smoke.f
;SUITE

SUITE engine-candidate-resolver
   test/engine-candidate-test.f
;SUITE

\ CPU tasking over pthread, including shared storage and task-local FFI staging.
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

SUITE zip
   lib/zip-test.f
   lib/zip-lifecycle-test.f
;SUITE

SUITE ffi-cabi
   lib/ffi-test.f
;SUITE

SUITE float-parse
   lib/float-test.f
   lib/fmath-test.f
;SUITE

SUITE finite-float-text
   lib/f64-text-test.f
   lib/f64-text-lifecycle-test.f
;SUITE

SUITE ieee-float32
   lib/ieee754-test.f
   lib/float32-test.f
   lib/float32-buffer-test.f
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

SUITE adt-option
   lib/adt/option-test.f
;SUITE

SUITE adt-result
   lib/adt/result-test.f
;SUITE

SUITE cad-num-arithmetic
   lib/cad-num-arithmetic-test.f
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

SUITE unicode-casefold
   lib/unicode-test.f
;SUITE

SUITE unicode-class-tools
   tools/unicode/class-tool-test.f
;SUITE

SUITE unicode-class-exhaustive
   tools/unicode/class-verify-main.f
;SUITE

SUITE-STDIN source-stdlib-stdin DATA
   lib/source-test.f -- stdin
;SUITE

SUITE argv-stdlib-mocks
   lib/argv-test.f
;SUITE

SUITE argv-stdlib-script-args
   lib/argv-test.f -- --json --label NAME --all-errors
   --strict-boundary -o OUT -- file.f --literal
;SUITE

SUITE test-stdlib
   lib/test/assert-test.f
   lib/test/suite-test.f
   lib/test/record-test.f
   lib/test/src-shape-test.f
   lib/test/mapped-test.f
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

SUITE signature-scan-emitter-shape
   tools/signature-scan-emitter-test.f
;SUITE

SUITE compiler-dispatch-shape
   tools/compiler-dispatch-test.f
;SUITE

SUITE compiler-compile-floor
   test/compiler/compile-floor.f
;SUITE

SUITE codegen-role
   tools/codegen-role-test.f
;SUITE

SUITE icode-fixup
   test/icode-fixup-test.f
;SUITE

SUITE aot-section-reach
   tools/aot-section-reach-lint-test.f
;SUITE

SUITE tail-pure-fixtures
   lib/json-write-test.f
   lib/json-read-test.f
   lib/memory-test.f
   lib/vector-test.f
   lib/byte-buffer-test.f
   lib/elf32-test.f
   lib/layout/box-test.f
   lib/fs-test.f
   tools/bootstrap-codegen-test.f
   tools/asm-src-test.f
   tools/asm-checked-test.f
   tools/image-bytes-test.f
;SUITE

SUITE xml-byte-edits
   lib/xml-test.f
   lib/byte-edit-test.f
   lib/xml-roundtrip-test.f
;SUITE

SUITE stdlib-source-default
   lib/source-test.f
;SUITE

SUITE stdlib-process-fixtures
   tools/hb-cli-contracts-test.f
   tools/standalone-load-test.f
   test/lint-cli-standalone-load.f
   lib/process-test.f
   lib/process-command-test.f
   lib/process-pty-handle-test.f
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

SUITE ptr-elem
   test/ptr-elem-test.f
;SUITE

SUITE typed-storage
   test/typed-storage-test.f
;SUITE

SUITE typed-storage-structural
   test/typed-storage-structural-test.f
;SUITE

SUITE certify-dynamic-buffer
   test/certify-dynamic-buffer.f
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

SUITE create-axiom
   test/create-axiom-test.f
;SUITE

SUITE ndict-spell-call
   test/ndict-spell-call.f
;SUITE

SUITE ndict-binding
   test/ndict-binding.f
;SUITE

SUITE checker-assert
   test/checker-assert-test.f
;SUITE

SUITE checker-verify-pkg-scope
   test/checker-verify-pkg-scope.f
;SUITE

SUITE checker-replay-pkg-state
   test/checker-replay-pkg-state.f
;SUITE

SUITE verify-prim
   test/verify-prim-test.f
;SUITE

SUITE checker-scan-index
   test/checker-scan-index-suite.f
;SUITE

SUITE defer-history
   test/defer-history.f
;SUITE

SUITE effect-intern
   test/effect-intern-suite.f
;SUITE

SUITE effect-store-census
   test/effect-store-census-test.f
;SUITE

SUITE checker-dead-path
   test/checker-dead-path-suite.f
;SUITE

SUITE checker-rollback-sig-pool
   test/checker-rollback-sig-pool.f
;SUITE

SUITE snapshot-writer
   test/snapshot-writer.f
;SUITE

SUITE stdlib-standalone-load
   test/stdlib-standalone-load.f
;SUITE

SUITE aot-wid-restore
   test/aot-wid-suite.f
;SUITE

SUITE aot-seed-batch
   test/aot-seed-batch-suite.f
;SUITE

SUITE aot-wide-format
   test/aot-wide-format-suite.f
;SUITE

SUITE aot-chain-capture
   test/aot-chain-capture-suite.f
;SUITE

SUITE aot-prelude-band
   test/aot-prelude-band-suite.f
;SUITE

SUITE aot-payload-admission
   test/aot-payload-admission.f
;SUITE

SUITE aot-source-identity
   test/aot-source-identity.f
;SUITE

SUITE aot-registry-identity
   test/aot-registry-identity.f
;SUITE

SUITE aot-payload-graph
   test/aot-payload-graph.f
;SUITE

SUITE aot-payload-unsupported
   test/aot-payload-unsupported.f
;SUITE

SUITE aot-sig-pool
   test/aot-sig-pool-suite.f
;SUITE

SUITE heap-start-cell
   test/heap-start-cell.f
;SUITE

SUITE compiler-native-create-does
   test/compiler/native-create-does.f
;SUITE

SUITE does-clause-record
   test/does-clause-record.f
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

SUITE snapshot-xt-cell-decl
   test/snapshot-xt-cell-decl.f
;SUITE

SUITE address-cell-cap-grown
   test/address-cell-cap-grown.f
;SUITE

SUITE catch-frame
   test/catch-frame.f
;SUITE

SUITE combinators
   test/combinators.f
;SUITE

SUITE export-keyword-package
   test/export-package.f
;SUITE

SUITE tokstream
   test/tokstream-suite.f
;SUITE

SUITE using-import
   test/using-test.f
;SUITE

SUITE trust-row-refusal
   test/trust-row-test.f
;SUITE

SUITE load-reject-diag
   test/load-reject-diag-test.f
;SUITE

SUITE dictionary-record-shapes
   test/drec-shape-test.f
;SUITE

SUITE core-prefix-mark
   test/prefix-mark-test.f
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

SUITE native-fixture-paths
   test/nf-path-test.f
;SUITE

SUITE load-argv-contract
   tools/load-argv-test.f
;SUITE

SUITE build-rewind
   test/build-rewind-test.f
;SUITE

SUITE cold-runtime
   test/cold-runtime-test.f
;SUITE

SUITE hb-build-fixtures
   tools/hb-build-test.f
   lib/build-cache-test.f
   lib/codesign-test.f
   tools/hb-build-direct-lints-test.f
;SUITE

SUITE gate-pool
   test/gate-pool-test.f
;SUITE

SUITE cad-num-types
   lib/cad-num-types-test.f
;SUITE

SUITE fs-mutate
   lib/fs-mutate-test.f
;SUITE

SUITE process-argv
   lib/process-argv-test.f
;SUITE

SUITE process-cwd
   lib/process-cwd-test.f
;SUITE

SUITE process-env
   lib/process-env-test.f
;SUITE

SUITE render
   lib/render-test.f
;SUITE

SUITE report
   lib/report-test.f
;SUITE

SUITE test-outcome
   lib/test/outcome-test.f
;SUITE

SUITE test-subject
   lib/test/subject-test.f
;SUITE

SUITE bootstrap-refresh-doc
   tools/bootstrap-refresh-doc-test.f
;SUITE

SUITE check-repair-hints
   tools/check-repair-hints-test.f
;SUITE

SUITE ddc-verify
   tools/ddc-verify-test.f
;SUITE

SUITE diff-side-content
   tools/diff-side-content-test.f
;SUITE

SUITE error-code-region
   tools/error-code-region-test.f
;SUITE

SUITE event-closure
   tools/event-closure-test.f
;SUITE

SUITE hb-baseline-contracts
   tools/hb-baseline-contracts-test.f
;SUITE

SUITE hb-open-failure
   tools/hb-open-failure-test.f
;SUITE

SUITE include-events
   tools/include-events-test.f
;SUITE

SUITE realpath
   test/realpath-test.f
;SUITE

SUITE source-root
   test/source-root-test.f
;SUITE

SUITE json
   tools/json-test.f
;SUITE

SUITE source-discovery
   tools/source-discovery-test.f
;SUITE

SUITE stdlib-time
   tools/stdlib-time-test.f
;SUITE

SUITE xref
   tools/xref-test.f
;SUITE

SUITE zed-run
   tools/zed-run-test.f
;SUITE

SUITE seed
   tools/seed-test.f
;SUITE

SUITE cast-negative
   test/cast-negative-suite.f
;SUITE

SUITE nominal-pointer
   test/nominal-pointer.f
;SUITE

SUITE pointer-view
   test/pointer-view.f
;SUITE

SUITE cast
   test/cast-suite.f
;SUITE

SUITE decl-event
   test/decl-event-suite.f
;SUITE

SUITE deftype
   test/deftype-suite.f
;SUITE

SUITE engine
   test/engine-suite.f
;SUITE

SUITE debugger-resume
   test/debugger-resume.f
;SUITE

SUITE engine-runtime-regressions
   test/runtime-regression-test.f
;SUITE

SUITE declaration-replay-source
   test/decl-replay-verify-source.f
;SUITE

SUITE program-diagnostics
   test/program-diagnostics-test.f
;SUITE

SUITE native-suite-cli
   test/run-cli-test.f
;SUITE

SUITE gate-env-stdin-tty
   test/gate-env-stdin-tty-test.f
;SUITE

SUITE match-factor-pin
   test/match-factor-pin.f
;SUITE

SUITE native-gate-debug
   test/gate-debug.f
;SUITE

SUITE native-gate-dictionary
   test/gate-dictionary.f
;SUITE

GROUP SEQ native-serial-gates

SUITE native-gate-diagnostics
   test/gate-diagnostics.f
;SUITE

SUITE native-gate-aot-positive
   test/gate-aot-positive.f
;SUITE

SUITE native-gate-aot-negative
   test/gate-aot-negative.f
;SUITE

;GROUP

SUITE type-layout-lower-pending
   test/type-layout-lower-pending.f
;SUITE

SUITE layout-buffer
   test/layout-buffer.f
;SUITE

SUITE dynamic-buffer
   test/dynamic-buffer.f
;SUITE

SUITE primitive-registry
   test/primitive-registry.f
;SUITE

SUITE dynamic-buffer-registry
   test/dynamic-buffer-registry.f
;SUITE

SUITE dynamic-buffer-capture
   test/dynamic-buffer-capture.f
;SUITE

SUITE dynamic-buffer-tasks
   test/dynamic-buffer-tasks.f
;SUITE

SUITE layout-defer
   test/layout-defer.f
;SUITE

SUITE lower-cert
   test/lower-cert.f
;SUITE

SUITE multi-error-api
   test/multi-error-api.f
;SUITE

SUITE layout-buffer-depth
   test/layout-buffer-depth.f
;SUITE

SUITE layout-valid-guards
   test/layout-valid-guards.f
;SUITE

SUITE layout-valid-growth
   test/layout-valid-growth.f
;SUITE

SUITE wide-store-seal
   test/wide-store-seal.f
;SUITE

SUITE protection-span
   test/protection-span.f
;SUITE

SUITE code-window
   test/code-window.f
;SUITE

SUITE addrmap-set
   test/addrmap-set.f
;SUITE

SUITE addrmap-call
   test/addrmap-call.f
;SUITE

SUITE p2-map-rewind
   test/p2-map-rewind.f
;SUITE

SUITE lower-txn-protection
   test/lower-txn-protection.f
;SUITE

SUITE lower-txn-large
   test/lower-txn-large.f
;SUITE

SUITE bootstrap-wide-memory-src
   test/bootstrap-wide-memory-src.f
;SUITE

SUITE enum-decl
   test/enum-decl-suite.f
;SUITE

SUITE extent-product
   test/extent-product-test.f
;SUITE

SUITE field-proj
   test/field-proj-suite.f
;SUITE

SUITE field-proj-boundary
   test/field-proj-boundary.f
;SUITE

SUITE gate-pool-orphan
   test/gate-pool-orphan-test.f
;SUITE

SUITE generated-declaration-transaction
   test/generated-declaration-transaction-suite.f
;SUITE

SUITE golden
   test/golden-test.f
;SUITE

SUITE lit-emit-size
   test/lit-emit-size-test.f
;SUITE

SUITE prop
   test/prop-test.f
;SUITE

SUITE require-cap
   test/require-cap-test.f
;SUITE

SUITE rigid-region
   test/rigid-region-suite.f
;SUITE

SUITE structure-certify
   test/structure-certify-suite.f
;SUITE

SUITE structure-decl
   test/structure-decl-suite.f
;SUITE

SUITE structure-make
   test/structure-make-suite.f
;SUITE

SUITE type-ctor
   test/type-ctor-suite.f
;SUITE

SUITE type-decl
   test/type-decl-suite.f
;SUITE

SUITE type-export
   test/type-export-suite.f
;SUITE

SUITE type-family-rollback
   test/type-family-rollback-suite.f
;SUITE

SUITE type-family
   test/type-family-suite.f
;SUITE

SUITE type-field-owner
   test/type-field-owner-suite.f
;SUITE

SUITE type-linear
   test/type-linear-suite.f
;SUITE

SUITE type-match
   test/type-match-suite.f
;SUITE

RUN

;using

s" PASS: native tests" type cr
