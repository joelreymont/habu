STDLIB-GATE:MAIN

using TEST

\ Registry order is start order: the compiler-images build starts in the pool
\ before these rows, and independent early rows run beside it. Each row waits
\ for a free slot or the images its load closure needs. Long rows lead so they
\ do not start late and alone set the gate's tail.

\ A ROW STAYS UNDER HALF ITS CPU BUDGET. The gate ends a row once its process
\ tree has run SUITE-BUDGET:CPU-MS (360 s) of CPU time, and its wall deadline
\ is only a hang guard (test/suite-budget.f). CPU time is the row's own work,
\ which load does not stretch as it does wall time, so a row is measured
\ outside the gate:
\ user plus system from `/usr/bin/time bin/hb --load <file>`, under 180 s.
\ native-file-unit, the longest, runs 115 s and c2-memory 113 s. Move a
\ fixture group into a new row file - the shared fixture stays in the family's
\ *-lib.f - rather than letting one row grow past that.

\ Diagnostic modes run many fresh checker processes and need no shared image.
\ Start them early so their serial checks overlap the other suite rows.
SUITE diag-position
   test/diag-position-test.f
;SUITE

SUITE source-root-exe
   test/source-root-exe-test.f
;SUITE

\ native-build-entry loads and captures the x86 window (about 55 s alone) and
\ needs no image, so it leads.
SUITE native-build-entry
   test/native-build-entry.f
;SUITE

\ The positive hb-build AOT checks are two rows of about 28 s each that share no
\ state and need no keyed image, so they start at once, beside the image build
\ rows, and add their work to the slots the whitebox hold below leaves idle.
SUITE native-gate-aot-positive-bundle
   test/gate-aot-positive-bundle.f
;SUITE

SUITE native-gate-aot-positive-preseed
   test/gate-aot-positive-preseed.f
;SUITE

\ The fixpoint fixtures reuse the suite's whitebox compiler image and the
\ fixture writer and cold host. This row waits for those images to settle.
\ The stripped-entry row waits for the saved AOT maker image.
SUITE build-fixpoint-fixtures
   tools/build-fixpoint-test.f
;SUITE

SUITE stripped-entry
   test/stripped-entry.f
;SUITE

SUITE ffi-callback-zero-image
   test/ffi-callback-zero-image.f
;SUITE

SUITE ffi-callback-full-image
   test/ffi-callback-full-image.f
;SUITE

SUITE ffi-callback-primitive-image
   test/ffi-callback-primitive-image.f
;SUITE

SUITE ffi-callback-file-cache
   test/ffi-callback-file-cache.f
;SUITE

\ Each C2 acceptance row waits for the compiler bundle, copies its rooted and
\ ordinary images into its owned source tree, then tests both consumers.
SUITE c2-memory
   test/c2-memory-e2e.f
;SUITE

SUITE c2-view-record
   test/c2-view-record-e2e.f
;SUITE

SUITE c2-field-loan
   test/c2-field-loan-e2e.f
;SUITE

SUITE hb-build-stripped
   tools/hb-build-stripped-test.f
;SUITE

SUITE hb-build-stripped-cells
   tools/hb-build-stripped-cells-test.f
;SUITE

SUITE hb-build-stripped-cache
   tools/hb-build-stripped-cache-test.f
;SUITE

SUITE aot-named-cells-image
   test/aot-named-cells-suite.f
;SUITE

SUITE hb-build-large-source
   tools/hb-build-large-source-test.f
;SUITE

SUITE aot-wide-format
   test/aot-wide-format-suite.f
;SUITE

SUITE aot-wide-prefix
   test/aot-wide-prefix-suite.f
;SUITE

SUITE stripped-entry-qualified
   test/stripped-entry-qualified.f
;SUITE

SUITE hb-build-fixtures
   tools/hb-build-test.f
   src/lib/build-cache-test.f
;SUITE

SUITE hb-build-stripped-chain
   tools/hb-build-stripped-chain-test.f
;SUITE

SUITE aot-chain-producer
   test/aot-chain-producer-suite.f
;SUITE

SUITE hb-build-stripped-lifecycle
   tools/hb-build-stripped-lifecycle-test.f
;SUITE

SUITE hb-build-stripped-quotation-field
   tools/hb-build-stripped-quotation-field-test.f
;SUITE

SUITE hb-build-cli-errors
   tools/hb-build-cli-errors-test.f
;SUITE

SUITE hb-build-timeout
   tools/hb-build-timeout-test.f
;SUITE

SUITE hb-build-timeout-env
   tools/hb-build-timeout-env-test.f
;SUITE

SUITE hb-build-timeout-json
   tools/hb-build-timeout-json-test.f
;SUITE

SUITE hb-build-repl-twin
   tools/hb-build-repl-twin-test.f
;SUITE

SUITE hb-build-repl-image
   tools/hb-build-repl-image-test.f
;SUITE

SUITE hb-build-repl-fresh
   tools/hb-build-repl-fresh-test.f
;SUITE

SUITE hb-build-aot
   tools/hb-build-aot-test.f
;SUITE

SUITE hb-build-aot-image
   tools/hb-build-aot-image-test.f
;SUITE

SUITE hb-build-aot-cache
   tools/hb-build-aot-cache-test.f
   tools/hb-build-producer-cache-test.f
   src/lib/codesign-test.f
   tools/hb-build-direct-lints-test.f
;SUITE

\ app-image uses the unsealed engine. whitebox-engine-build supplies its
\ private copy from the initial compiler bundle; later consumers use that copy.
SUITE app-image
   test/app-image.f
;SUITE

SUITE native-file-state
   test/native-file-object-prepared-e2e.f
   test/native-file-prepared-boundary-e2e.f
   test/native-file-state-e2e.f
   test/native-file-retired-e2e.f
;SUITE

SUITE native-file-types
   test/native-file-types-e2e.f
;SUITE

SUITE native-file-stack
   test/native-file-stack-e2e.f
;SUITE

SUITE native-file-auto
   test/native-file-auto-e2e.f
   test/native-file-cwork-e2e.f
   test/native-file-admission-e2e.f
;SUITE

SUITE native-file-replay
   test/native-file-inputs-format.f
   test/native-file-inputs-empty.f
   test/native-file-replay-e2e.f
   test/native-file-path-shadow-e2e.f
;SUITE

SUITE native-file-dynamic-load
   test/native-file-dynamic-load-e2e.f
;SUITE

SUITE native-file-missing
   test/native-file-missing-e2e.f
;SUITE

SUITE source-unit-range
   test/source-unit-range-gate.f
;SUITE

SUITE native-file-prefix
   tools/hb-build-prefix-producer-test.f
;SUITE

SUITE native-file-alias
   test/native-file-alias-e2e.f
;SUITE

SUITE native-file-stripped-path
   test/native-file-stripped-path-e2e.f
;SUITE

\ Each FILE pair builds twice; separate rows keep the native gate CPU budget.
SUITE native-file-forms-zero
   test/native-file-forms-zero.f
;SUITE

SUITE native-file-forms-namespace
   test/native-file-forms-namespace.f
;SUITE

SUITE native-file-forms-global
   test/native-file-forms-global.f
;SUITE

SUITE native-file-forms-repeat
   test/native-file-forms-repeat.f
;SUITE

\ The explicit FILE unit and FFI rows spawn builds on the unsealed engine
\ (test/whitebox-engine.f) and stay plain SUITE rows; loading that module
\ makes each row wait for whitebox-engine-build.
SUITE native-file-unit
   test/native-file-unit-e2e.f
;SUITE

SUITE native-file-ffi
   test/native-file-ffi-e2e.f
;SUITE

SUITE native-unit-refusals
   test/native-unit-key-e2e.f
   test/native-unit-file.f
   test/native-file-object-wid-reject.f
   test/native-unit-compile-e2e.f
;SUITE

\ A package's checker facts survive an artifact boundary.
SUITE checker-unit-codec
   test/checker-unit-codec-e2e.f
;SUITE

SUITE checker-unit-capture
   test/checker-unit-capture-run.f
;SUITE

SUITE native-window-owner
   test/native-window-owner.f
;SUITE

SUITE field-proj-boundary
   test/field-proj-boundary.f
;SUITE

WHITEBOX-SUITE prop
   test/prop-test.f
;SUITE

WHITEBOX-SUITE checker-scan-index
   test/checker-scan-index-suite.f
;SUITE

WHITEBOX-SUITE checker-scan-index-rollback
   test/checker-scan-index-rollback-suite.f
;SUITE

SUITE hb-build-retain
   tools/hb-build-retain-test.f
;SUITE

SUITE build-fixpoint-source
   tools/build-fixpoint-source-test.f
;SUITE

SUITE aot-chain-capture
   test/aot-chain-capture-suite.f
;SUITE

SUITE aot-owned
   test/aot-owned.f
;SUITE

SUITE build-fixpoint-sandbox
   tools/build-fixpoint-sandbox-test.f
;SUITE

SUITE os-memory
   src/lib/os-memory-test.f
;SUITE

SUITE shadow-lint
   tools/lint/shadow-lint.f
   tools/lint/shadow-lint-test.f
;SUITE

SUITE bare-copy-lint
   tools/lint/bare-copy-lint.f
   tools/lint/bare-copy-lint-test.f
;SUITE

SUITE clobber-lint
   tools/lint/clobber-lint.f
   tools/lint/clobber-lint-test.f
;SUITE

SUITE dot-dep-lint-fixtures
   tools/dot-dep-lint-test.f
;SUITE

SUITE error-code-lint-fixtures
   tools/error-code-lint-test.f
;SUITE

SUITE manifest-lint-fixtures
   tools/manifest-lint-test.f
;SUITE

SUITE chain-plan
   tools/chain-plan-test.f
;SUITE

SUITE chain-run
   tools/chain-run-test.f
;SUITE

SUITE text-foundation-fixtures
   tools/lint/text-foundation-test.f
;SUITE

SUITE lint-slab
   tools/lint/slab-test.f
;SUITE

SUITE byte-copy
   test/byte-copy-test.f
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

SUITE imgdump-compare
   tools/imgdump-test.f
;SUITE

SUITE engine-size-fixtures
   tools/engine-size-test.f
;SUITE

SUITE two-generation-fixtures
   tools/two-generation-test.f
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

SUITE live-verify-source
   test/live-verify-source-e2e.f
;SUITE

SUITE check-verify
   tools/check-verify-test.f
;SUITE

SUITE verify-binding
   test/verify-binding.f
;SUITE

SUITE lsp-boundary
   tools/lsp-test.f
   tools/lsp-entry-test.f
;SUITE

SUITE primitive-source
   test/primitive-source.f
;SUITE

SUITE check-verify-batch
   tools/check-verify-batch-test.f
;SUITE

SUITE lsp-batch
   tools/lsp-batch-test.f
;SUITE

SUITE lsp-prefix
   tools/lsp-prefix-test.f
;SUITE

SUITE streaming-sha256
   tools/sha256-file-test.f
;SUITE

SUITE content-key
   src/lib/content-key-test.f
;SUITE

SUITE engine-identity
   src/lib/engine-id-test.f
;SUITE

SUITE engine-id-capture
   test/engine-id-capture-e2e.f
;SUITE

SUITE engine-id-replace
   test/engine-id-replace-e2e.f
;SUITE

WHITEBOX-SUITE compiler-ir-id
   test/compiler/ir-id.f
;SUITE

SUITE compiler-ir-id-manifest
   test/compiler/ir-id-manifest.f
;SUITE

\ These manifests execute the native schema and case checks without a prover.
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

WHITEBOX-SUITE compiler-native-tape-owner
   test/compiler/native-tape-owner.f
;SUITE

WHITEBOX-SUITE compiler-native-host-construction
   test/compiler/native-host-construction-e2e.f
;SUITE

SUITE compiler-native-string
   test/compiler/native-string.f
;SUITE

SUITE compiler-native-string-forms
   test/compiler/native-string-forms.f
;SUITE

SUITE compiler-native-long-string-image
   test/compiler/native-long-string-image.f
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

\ Reads the code the ENGINE UNDER TEST baked, so it belongs to whichever engine
\ the gate runs and not to the sources a fixture could compile for itself.
SUITE compiler-tier1-inline-prims
   test/compiler/tier1-inline-prims.f
;SUITE

\ The writeback addressing modes, beside the package boundary that publishes
\ them. They are the one-instruction spelling of every pointer move the engine
\ and the native compiler make, so a wrong mode bit is a wrong stack.
SUITE compiler-a64-indexed
   test/compiler/a64-indexed.f
;SUITE

SUITE compiler-native-a64ir
   test/compiler/native-a64ir.f
;SUITE

\ The typed ARM64 routine-effect schema, next to the a64 lowering it constrains.
\ It was fork-only, so the register bounds it pins were unchecked in a standalone
\ gate run.
SUITE compiler-native-effect
   test/compiler/native-effect.f
;SUITE

SUITE compiler-native-colon
   test/compiler/native-colon.f
;SUITE

\ The target/policy binding: src/compiler/digest.f, target.f, numeric-policy.f
\ and binding.f through their public words. It is the acceptance suite
\ habu-bind-compiler-target-b3dfa307 is answered by, and a suite that answers a
\ dot has to be reachable by name in the registry, not only inside a fork list -
\ that missing row is what blocked the dot.
SUITE compiler-target-resolve
   test/compiler/target-resolve.f
;SUITE

SUITE compiler-target-policy
   test/compiler/target-policy.f
;SUITE

SUITE compiler-target-digest-goldens
   test/compiler/target-digest-goldens.f
;SUITE

SUITE compiler-target-native-layout-goldens
   test/compiler/target-native-layout-goldens.f
;SUITE

\ The sealed Wasm target, scalar-FP split and function convention.
SUITE compiler-wasm-target
   test/compiler/wasm-target.f
;SUITE

\ The N01-N06 and W32 numeric rows of test/wasm/numeric-rows.f, each forked and
\ its printed bytes compared, on the default tier and the optimizing compiler.
SUITE wasm-numeric
   test/wasm/numeric.f
;SUITE

SUITE wasm-numeric-aot
   src/lib/test.f
   src/lib/string.f
   src/lib/test/outcome.f
   src/lib/test/subject.f
   src/lib/ieee754.f
   test/compiler/aot-mode.f
   ENTRIES
   test/wasm/numeric.f
;SUITE

\ WLEB, the Wasm backend's LEB128: the shortest and padded forms at their
\ edges, the call and address fields rewritten in place, and each refusal.
SUITE wasm-leb
   test/wasm/leb.f
;SUITE

\ The Wasm backend's dialect, src/arch/wasm/wstruct.f: its closed vocabulary,
\ a module frozen through WSTRUCT:FREEZE and the substrate's refusals.
SUITE wasm-wstruct
   test/wasm/wstruct.f
;SUITE

\ WCTL, the Wasm backend's structured control, src/arch/wasm/structure.f: W01's
\ copies and label depths over nested loops and ifs, and W02's refusal of a
\ cycle entered at two blocks.
SUITE wasm-structure
   test/wasm/structure.f
;SUITE

\ The Wasm selector, src/arch/wasm/select.f: definitions compiled through
\ NBACK and selected to frozen WSTRUCT, calls, status and the integer rows.
SUITE wasm-select
   test/wasm/select.f
;SUITE

\ WENC, the Wasm backend's encoder, src/arch/wasm/encode.f: byte-exact bodies
\ for a straight line, an if/else, nested loops, calls and address literals,
\ the emission's header, call and address sites, and each refusal.
SUITE wasm-encode
   test/wasm/encode.f
;SUITE

\ WLINK, the Wasm backend's linker, src/arch/wasm/link.f: modules wasm-tools
\ assembles byte for byte, index and type order, patched call and address
\ fields, the table, data cells and memory pages, W03 and each refusal.
SUITE wasm-link
   test/wasm/link.f
;SUITE

\ WSEL's f64 rows: Habu's NaN rule after f64 add, sub, mul, div and sqrt, the
\ sign operations, the comparisons and the conversions.
SUITE wasm-select-f64
   test/wasm/select-f64.f
;SUITE

\ The Wasm backend registered at run time, src/arch/wasm/backend.f and
\ passes.f: real source compiled through an open Wasm shadow, one emission per
\ record whose rows match its bytes, NEMIT empty after each, the placed row
\ refused, and the engine's own routine unchanged.
SUITE wasm-backend
   test/wasm/backend.f
;SUITE

\ The capture's Wasm reader, src/arch/wasm/capture.f: a window compiled through
\ an open Wasm shadow, captured, its emissions read into AOT-SHADOW's tables
\ with each call row, code and DATA field and code cell keyed by shipped row.
SUITE wasm-capture
   test/wasm/capture.f
;SUITE

\ src/arch/wasm/kernel-words.f beside the engine's primitives on the host: each
\ engine name WKERNEL's map answers with a word prints and exits as the engine
\ primitive does, and a name no provider answers is refused.
SUITE wasm-kernel-words
   test/wasm/kernel.f
;SUITE

\ WASMLINK, src/habu/link-wasm.f: a captured window linked into one module whose
\ data image is the window's DATA, and the three windows it refuses by name.
SUITE wasm-link-capture
   test/wasm/link-capture.f
;SUITE

\ The backend registry: complete rows in src/compiler/native/backend.f,
\ registered by src/arch/arm64/passes.f, which is the acceptance suite for
\ habu-bind-compiler-targets-ff970b99.
SUITE compiler-target-registry
   test/compiler/target-registry.f
;SUITE

SUITE compiler-session
   test/compiler/session.f
;SUITE

SUITE compiler-arm32-asm
   test/compiler/arm32-asm.f
;SUITE

SUITE compiler-x86-64-asm
   test/compiler/x86-64-asm.f
;SUITE

\ The x86-64 machine dialect and the capability predicate it loads with:
\ src/compiler/native/x64ir.f and src/arch/x86-64/backend.f.
SUITE compiler-x64ir
   test/compiler/x64ir.f
;SUITE

\ The x86-64 selection pass: src/compiler/native/select-x64.f and the routine
\ contracts it is told, src/arch/x86-64/abi.f.
SUITE compiler-x64-select
   test/compiler/x64-select.f
;SUITE

\ The register allocator and its validator over an x86-64 module: the same
\ src/compiler/native/regalloc.f and regalloc-verify.f the ARM64 suite runs,
\ reading this machine through the vocabulary src/compiler/native/dialect.f
\ declares and src/compiler/native/x64ir.f builds.
SUITE compiler-x64-regalloc
   test/compiler/x64-regalloc.f
;SUITE

\ The x86-64 bytes themselves: src/compiler/native/emit-x64.f over the modules
\ the two suites above select and accept, pinned against llvm-mc vectors.
SUITE compiler-x64-emit
   test/compiler/x64-emit.f
;SUITE

\ The x86-64 pass chain: the rows src/arch/x86-64/passes.f installs in
\ src/compiler/native/backend.f, driven as src/compiler/native/compiler.f drives
\ them, with the shared spill pass lowering the allocator's plan.
SUITE compiler-x64-chain
   test/compiler/x64-chain.f
;SUITE

\ A second target beside the engine's own: the driver compiles each definition
\ through the x86-64 rows in a nested context while an NSHADOW is open, and the
\ map src/compiler/native/shadow.f keeps is read back from this host.
SUITE compiler-shadow
   test/compiler/shadow.f
;SUITE

SUITE compiler-tic6x-asm
   test/compiler/tic6x-asm.f
   test/compiler/tic6x-facts.f
   test/compiler/tic6x-sim.f
   test/compiler/tic6x-eabi.f
;SUITE

SUITE compiler-backend-boundary
   test/compiler/backend-boundary.f
;SUITE

SUITE compiler-native-select
   test/compiler/native-select.f
;SUITE

\ The register-file description the allocator reads instead of one architecture's
\ constants, and the ARM64 description a64ir.f derives from it. It is beside the
\ allocator's own suite because the allocator is its only production consumer and
\ the two have to agree about what a machine is.
SUITE compiler-regfile
   test/compiler/regfile.f
;SUITE

SUITE compiler-native-regalloc
   test/compiler/native-regalloc.f
;SUITE

SUITE compiler-native-address-spill
   test/compiler/native-address-spill.f
;SUITE

SUITE compiler-native-identity-spill
   test/compiler/native-identity-spill.f
;SUITE

SUITE compiler-native-loop-frame-order
   test/compiler/native-loop-frame-order.f
;SUITE

SUITE compiler-native-arm-frame-order
   test/compiler/native-arm-frame-order.f
;SUITE

SUITE compiler-native-wide-frame
   test/compiler/native-wide-frame.f
;SUITE

SUITE compiler-native-emit
   test/compiler/native-emit.f
;SUITE

SUITE compiler-native-fused-moves
   test/compiler/native-fused-moves.f
;SUITE

SUITE compiler-native-session
   test/compiler/native-session.f
;SUITE

SUITE compiler-native-trap
   test/compiler/native-trap.f
;SUITE

SUITE compiler-native-div-refusal
   test/compiler/native-div-refusal.f
;SUITE

SUITE compiler-native-quot
   test/compiler/native-quot.f
;SUITE

SUITE compiler-native-defer
   test/compiler/native-defer.f
;SUITE

SUITE compiler-native-finally
   test/compiler/native-finally.f
;SUITE

SUITE compiler-native-finally-aot
   src/lib/test.f
   test/checker-assert.f
   test/compiler/aot-mode.f
   ENTRIES
   test/compiler/native-finally.f
;SUITE

SUITE compiler-native-layout-control
   test/compiler/native-layout-control.f
;SUITE

SUITE compiler-native-layout-control-aot
   test/compiler/aot-mode.f
   ENTRIES
   test/compiler/native-layout-control.f
;SUITE

SUITE compiler-native-fetch-check
   test/compiler/native-fetch-check.f
;SUITE

SUITE compiler-native-fetch-snapshot
   test/compiler/native-fetch-snapshot.f
;SUITE

SUITE compiler-native-prefix-declarations
   test/compiler/native-prefix-declarations.f
;SUITE

SUITE primitive-trust
   test/primitive-trust.f
;SUITE

\ Native primitive goldens live in prim-cases.f and prim-float-cases.f, owned
\ by prim-parity.f. They pin wrap at MAX-N+1, MIN-N/-1, divisor throws,
\ modulo-64 shifts, and canonical NaN outputs through compiled primitive calls.
\ Each case runs against this backend and its available reference implementation
\ on the product engine.
SUITE prim-parity
   test/prim-parity.f
;SUITE

\ The engine's own `.` and `u.` over MIN-N, read back through a genio device.
\ The parity gate cannot hold these: a printer's answer is text on a device, not
\ a cell a case column can state.
SUITE print-min-int
   test/print-min-int.f
;SUITE

\ WHITEBOX-SUITE runs the file on the unsealed engine test/whitebox-engine.f
\ builds once per gate, never on bin/hb. Declare one when the suite reaches
\ inside the engine it tests - a reopened engine package, a pre-hook global, a
\ ticked internal word - and say here which of those it needs.
\ test/internal-word-gate.f stays on the product engine on purpose: it pins the
\ refusals this engine does not have. test/compiler/ir-id.f does not - it is the
\ compiler-ir-id row above, because its FAMILY-SURFACE and PUBLIC-SURFACE cases
\ read the engine's own TFAM and XREF tables (standalone on bin/hb: rc 1, 26
\ failures).
WHITEBOX-SUITE whitebox-engine
   test/whitebox-engine-suite.f
;SUITE

\ PRIM: / PPRIM: / CLOSE-PRIVATE and the owner packages the fixture opens are
\ sealed in the product image, which is why this suite drives its fixture
\ through a from-source child window that runs the production seal.
SUITE prim-owner-scope
   test/prim-owner-scope.f
;SUITE

WHITEBOX-SUITE checker-effect-authority
   test/checker-effect-authority.f
;SUITE

SUITE control-capture
   test/control-capture.f
;SUITE

SUITE name-intern-capture
   test/name-intern-capture.f
;SUITE

SUITE loop-obligations
   test/loop-obligations.f
;SUITE

SUITE native-build-layout
   test/native-layout.f
;SUITE

SUITE stripped-image
   test/stripped-image.f
;SUITE

SUITE stripped-lifecycle-prepare
   test/stripped-lifecycle-prepare.f
;SUITE

\ These three rows run on the linker image (test/preloaded-engine.f). The first
\ row that needs it holds the registry until app-image-build and linker-build
\ pass, so the others add no hold of their own.
SUITE stripped-address
   test/stripped-address.f
;SUITE

SUITE native-gate-aot-negative
   test/gate-aot-negative.f
;SUITE

SUITE ffi-growth
   test/ffi-growth.f
;SUITE

SUITE stripped-preloaded-runtime
   test/stripped-preloaded-runtime.f
;SUITE

SUITE stripped-literal
   test/stripped-literal.f
;SUITE

SUITE aot-data-cell-refusals
   test/compiler/aot-data-cell-refusals.f
;SUITE

SUITE aot-seeded-address-sites
   test/aot-seeded-address-sites.f
;SUITE

SUITE compiler-native-exec
   test/compiler/native-exec.f
;SUITE

SUITE native-quote-forward
   test/native-quote-forward-e2e.f
;SUITE

SUITE compiler-native-generic-calls
   test/compiler/native-generic-calls.f
;SUITE

SUITE compiler-native-generic-calls-aot
   src/lib/test.f
   test/compiler/aot-mode.f
   ENTRIES
   test/compiler/native-generic-calls.f
;SUITE

SUITE compiler-native-provider-rows
   test/compiler/native-provider-rows.f
;SUITE

SUITE compiler-native-internal-call
   test/compiler/native-internal-call.f
;SUITE

SUITE compiler-native-hookless-reject
   test/compiler/native-hookless-reject.f
;SUITE

SUITE compiler-native-stored-quot
   test/compiler/native-stored-quot.f
;SUITE

SUITE compiler-native-stored-quot-aot
   src/lib/test.f
   test/compiler/aot-mode.f
   ENTRIES
   test/compiler/native-stored-quot.f
;SUITE

SUITE image-lifecycle
   test/image-lifecycle.f
;SUITE

SUITE image-lifecycle-tasks
   test/image-lifecycle-tasks.f
;SUITE

SUITE image-lifecycle-late-register
   test/image-lifecycle-late-register.f
;SUITE

SUITE native-resource-image
   test/native-resource-image.f
;SUITE

WHITEBOX-SUITE registry-persist
   test/registry-persist.f
;SUITE

SUITE native-defer-image
   test/native-defer-image.f
;SUITE

SUITE process-image
   test/process-image.f
;SUITE

\ Serial and UDP workers through three saved generations, each a fresh
\ process, with the pty master and the UDP peer as tasks in this row. Runs on
\ the saver image like process-image.
SUITE host-io-image
   test/host-io-image.f
;SUITE

SUITE compiler-native-many-locals
   test/compiler/native-many-locals.f
;SUITE

SUITE compiler-native-many-locals-aot
   src/lib/test.f
   test/compiler/aot-mode.f
   ENTRIES
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

SUITE native-branch-reloc
   test/native-branch-reloc.f
;SUITE

SUITE compiler-native-tail
   test/compiler/native-tail.f
;SUITE

SUITE compiler-native-fold
   test/compiler/native-fold.f
;SUITE

SUITE compiler-native-loop
   test/compiler/native-loop.f
;SUITE

SUITE compiler-jit-plusloop
   test/compiler/jit-plusloop.f
;SUITE

SUITE compiler-jit-do
   test/compiler/jit-do.f
;SUITE

SUITE compiler-native-edge-permutation
   test/compiler/native-edge-permutation.f
;SUITE

SUITE compiler-native-switch
   test/compiler/native-switch.f
;SUITE

SUITE compiler-native-switch-aot
   src/lib/test.f
   test/compiler/aot-mode.f
   ENTRIES
   test/compiler/native-switch.f
;SUITE

SUITE compiler-native-do
   test/compiler/native-do.f
;SUITE

SUITE compiler-native-plusloop
   test/compiler/native-plusloop.f
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

SUITE compiler-native-rstack-input
   test/compiler/native-rstack-input.f
;SUITE

SUITE compiler-native-catch
   test/compiler/native-catch.f
;SUITE

SUITE finally
   test/finally.f
;SUITE

SUITE empty-quotation
   test/empty-quotation.f
;SUITE

SUITE compiler-native-locals-scope
   test/compiler/native-locals-scope.f
;SUITE

SUITE compiler-native-locals-scope-aot
   test/compiler/native-eval-fixture.f
   src/lib/test.f
   test/compiler/aot-mode.f
   ENTRIES
   test/compiler/native-locals-scope.f
;SUITE

SUITE compiler-native-local-case
   test/compiler/native-local-case.f
;SUITE

SUITE compiler-native-local-ambiguity
   test/compiler/native-local-ambiguity.f
;SUITE

SUITE compiler-native-product-locals
   test/compiler/native-product-locals.f
;SUITE

SUITE compiler-native-product-locals-aot
   src/lib/test.f
   test/compiler/aot-mode.f
   ENTRIES
   test/compiler/native-product-locals.f
;SUITE

SUITE compiler-native-word-binding
   test/compiler/native-word-binding.f
;SUITE

SUITE compiler-native-call-binding
   test/compiler/native-call-binding.f
;SUITE

SUITE compiler-native-word-binding-aot
   src/lib/test.f
   test/compiler/aot-mode.f
   ENTRIES
   test/compiler/native-word-binding.f
;SUITE

SUITE reopen-binding
   test/reopen-binding.f
;SUITE

SUITE reopen-binding-aot
   src/lib/test.f
   test/compiler/aot-mode.f
   ENTRIES
   test/reopen-binding.f
;SUITE

SUITE replay-binding
   test/replay-binding.f
;SUITE

SUITE undefine-binding
   test/undefine-binding.f
;SUITE

SUITE undefine-binding-aot
   src/lib/test.f
   test/compiler/aot-mode.f
   ENTRIES
   test/undefine-binding.f
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

WHITEBOX-SUITE compiler-native-dictionary-retarget
   test/compiler/native-dictionary-retarget.f
;SUITE

SUITE compiler-code-span
   test/compiler/code-span.f
;SUITE

SUITE compiler-code-span-capture
   test/compiler/code-span-capture.f
;SUITE

SUITE compiler-code-bytes
   test/compiler/code-bytes.f
;SUITE

SUITE compiler-native-code-span
   test/compiler/native-code-span.f
;SUITE

SUITE compiler-aot-nested-body
   test/compiler/aot-nested-body.f
;SUITE

SUITE compiler-aot-closure-index
   test/compiler/aot-closure-index.f
;SUITE

SUITE compiler-native-quot-scope
   test/compiler/native-quot-scope.f
;SUITE

SUITE compiler-native-quot-scope-aot
   src/lib/test.f
   test/checker-assert.f
   test/compiler/aot-mode.f
   ENTRIES
   test/compiler/native-quot-scope.f
;SUITE

SUITE compiler-native-chain
   test/compiler/native-chain.f
;SUITE

SUITE compiler-native-chain-aot
   src/lib/test.f
   test/compiler/aot-mode.f
   ENTRIES
   test/compiler/native-chain.f
;SUITE

SUITE compiler-native-qualified-trusted
   test/compiler/native-qualified-trusted.f
;SUITE

SUITE compiler-native-generated-constructor
   test/compiler/native-generated-constructor.f
;SUITE

SUITE compiler-native-generated-constructor-aot
   test/compiler/aot-mode.f
   ENTRIES
   test/compiler/native-generated-constructor.f
;SUITE

SUITE compiler-native-order-exit
   test/compiler/native-order-exit.f
;SUITE

SUITE compiler-native-exit
   test/compiler/native-exit.f
;SUITE

SUITE compiler-native-exit-aot
   src/lib/test.f
   test/compiler/aot-mode.f
   ENTRIES
   test/compiler/native-exit.f
;SUITE

SUITE compiler-native-tick
   test/compiler/native-tick.f
;SUITE

SUITE compiler-native-tick-aot
   src/lib/test.f
   test/compiler/aot-mode.f
   ENTRIES
   test/compiler/native-tick.f
;SUITE

SUITE compiler-native-literals
   test/compiler/native-literals.f
;SUITE

SUITE compiler-native-literals-aot
   src/lib/test.f
   test/compiler/aot-mode.f
   ENTRIES
   test/compiler/native-literals.f
;SUITE

SUITE compiler-integer-literals
   test/compiler/integer-literals.f
;SUITE

SUITE outer-number
   test/outer-number.f
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

WHITEBOX-SUITE compiler-native-match
   test/compiler/native-match.f
;SUITE

WHITEBOX-SUITE compiler-native-match-aot
   test/compiler/native-eval-fixture.f
   src/lib/test.f
   src/lib/process.f
   src/lib/process-argv.f
   src/lib/process-env.f
   test/compiler/aot-mode.f
   ENTRIES
   test/compiler/native-match.f
;SUITE

SUITE compiler-native-case
   test/compiler/native-case.f
;SUITE

SUITE compiler-native-rename-rows
   test/compiler/native-rename-rows.f
;SUITE

\ A read-view unpacked by a CAST: used as its two cells: the base row is the
\ tier-0 reference, its -aot twin the tier-1 regrouping (test/compiler/aot-mode.f).
SUITE compiler-native-view-cast
   test/compiler/native-view-cast.f
;SUITE

SUITE compiler-native-view-cast-aot
   src/lib/test.f
   src/lib/memory.f
   src/lib/c2-memory.f
   test/compiler/aot-mode.f
   ENTRIES
   test/compiler/native-view-cast.f
;SUITE

SUITE compiler-native-wide-mem
   test/compiler/native-wide-mem.f
;SUITE

SUITE compiler-native-wide-mem-aot
   test/compiler/native-eval-fixture.f
   src/lib/test.f
   src/lib/ieee754.f
   test/compiler/aot-mode.f
   ENTRIES
   test/compiler/native-wide-mem.f
;SUITE

SUITE compiler-native-fetch-terms
   test/compiler/native-fetch-terms.f
;SUITE

SUITE compiler-native-fetch-terms-aot
   src/lib/test.f
   test/compiler/aot-mode.f
   ENTRIES
   test/compiler/native-fetch-terms.f
;SUITE

SUITE compiler-native-vocab
   test/compiler/native-vocab.f
;SUITE

SUITE raw-storage-load-seal
   test/raw-storage-load-seal-test.f
;SUITE

SUITE raw-cell-pointer-refusals
   test/compiler/raw-cell-pointer-refusals.f
;SUITE

SUITE input-underflow-refusals
   test/compiler/input-underflow-refusals.f
;SUITE

SUITE base-pointer-arith-refusals
   test/compiler/base-pointer-arith-refusals.f
;SUITE

SUITE object-record-codec
   src/lib/object-test.f
;SUITE

SUITE object-cache-store
   src/lib/object-cache-test.f
;SUITE

SUITE object-source-index
   src/lib/object-index-test.f
;SUITE

SUITE object-source-resolver
   src/lib/object-resolve-test.f
;SUITE

SUITE object-link-symbols
   src/lib/object-link-test.f
;SUITE

SUITE object-image-writer
   tools/object-image-test.f
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

SUITE proc-capture-under-signals
   test/proc-capture-signal.f
;SUITE

SUITE process-fork-wrappers
   src/lib/process-fork-test.f
   src/lib/fork-child-test.f
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
   src/lib/task-test.f
;SUITE

\ Readiness, timers and cancellation on io_uring, over pipes only.
SUITE aio-uring
   src/lib/aio-test.f
;SUITE

\ Process signals on the engine's baked stub and a self-pipe.
SUITE process-signals
   src/lib/signal-test.f
;SUITE

\ The bounded queue: many producers and many consumers over one ring.
SUITE bounded-queue
   src/lib/queue-test.f
;SUITE

SUITE task-entry
   test/task-entry.f
;SUITE

\ Exercise owner cleanup through the checked public allocation path.
SUITE c2-owner-dispose
   test/c2-owner-dispose.f
;SUITE

SUITE string-helpers
   src/lib/string-test.f
;SUITE

SUITE utf8-scalar
   src/lib/utf8-scalar-test.f
;SUITE

SUITE base64
   src/lib/base64-test.f
;SUITE

SUITE utf16-units
   src/lib/utf16-test.f
;SUITE

SUITE file-uri
   src/lib/uri-test.f
;SUITE

\ Exact reads and full writes over pipes; the write cases run under the
\ profiler's SIGALRM storm to force short writes.
SUITE fd-io
   src/lib/fd-io-test.f
;SUITE

SUITE content-length
   src/lib/content-length-test.f
;SUITE

SUITE json-rpc
   src/lib/json-rpc-test.f
;SUITE

SUITE ffi-abi
   src/lib/ffi-abi-test.f
;SUITE

SUITE zip
   src/lib/zip-test.f
   src/lib/zip-lifecycle-test.f
   test/zip-remove-source.f
;SUITE

SUITE zip-xml-consumer
   test/zip-xml-source.f
;SUITE

SUITE zip-temp
   test/zip-temp-e2e.f
;SUITE

SUITE ffi-cabi
   src/lib/ffi-test.f
;SUITE

\ C calling back into checked code through libc's qsort and pthread_create, on
\ the calling task and on foreign threads; writes
\ build/ffi-callback-transcript.txt and compares it whole.
SUITE ffi-callback
   src/lib/ffi-callback-test.f
;SUITE

\ More than 129 native C entries, stable pointers through growth, high-row
\ lifecycle and fallback; the emitter check reaches beyond a 16-bit slot ID.
SUITE ffi-callback-growth
   test/ffi-callback-growth.f
;SUITE

SUITE native-callback-stub-immediate
   test/native-callback-stub-immediate.f
;SUITE

\ IMAGE-LIFECYCLE:PREPARE closes the library a FUNCTION: row opened, and the
\ row's next call opens it again; RTLD_NOLOAD reads whether libzip is loaded.
SUITE ffi-library-lifetime
   test/ffi-library-lifetime.f
;SUITE

\ The five foreign libraries a server binds at once, in one image: the load is
\ the case, because one short declaration table refuses it outright.
SUITE five-bindings
   test/five-bindings.f
;SUITE

\ Packages PG and DB-ROWS against a private PostgreSQL cluster the harness
\ starts on a Unix-domain socket, runs src/lib/pg-test.f and then src/lib/db/rows-test.f
\ against, and stops however they end; then the same harness killed by a pool,
\ which must leave no server process and no socket directory. initdb and
\ postgres on PATH are a gate requirement (docs/bootstrap.md). The harness's
\ step deadlines sum to 300 s, inside the row's hang guard (test/suite-budget.f
\ ROW-MS).
SUITE pg
   test/db/pg-cluster.f
   test/db/pg-kill-test.f
;SUITE

\ PG's configuration lock in a child forked while another task holds it
\ (src/lib/fork-child.f). Loading src/lib/pg.f opens libpq; no server is needed.
SUITE pg-fork
   src/lib/pg-fork-test.f
;SUITE

\ Loopback HTTP in one process; the one HTTPS request is opt-in behind
\ HABU_NET_TESTS.
SUITE curl-http
   src/lib/net/curl-test.f
;SUITE

\ The HTTP/1.1 server on a loopback port, answered by CURL and by raw TCP4;
\ writes build/http-transcript.txt and compares it whole.
SUITE http
   src/lib/net/http-test.f
;SUITE

\ BROWSER-HOST:ROUTES on a loopback port: CURL fetches the browser host's page,
\ each file's content type and bytes.
SUITE browser-host
   src/lib/browser/host-test.f
;SUITE

SUITE crypto-evp
   src/lib/crypto/evp-test.f
;SUITE

SUITE crypto-sha1
   src/lib/crypto/sha1-test.f
;SUITE

SUITE float-parse
   src/lib/float-test.f
   src/lib/fmath-test.f
   src/lib/fmath-basis-test.f
;SUITE

SUITE finite-float-text
   src/lib/f64-text-test.f
   src/lib/f64-text-lifecycle-test.f
;SUITE

SUITE ieee-float32
   src/lib/ieee754-test.f
   src/lib/float32-test.f
   src/lib/float32-buffer-test.f
;SUITE

SUITE fmt-numbers
   src/lib/fmt-test.f
;SUITE

SUITE float-sort
   src/lib/sort-test.f
;SUITE

SUITE hashmap
   src/lib/hashmap-test.f
;SUITE

SUITE prelude
   src/lib/prelude-test.f
;SUITE

SUITE array-helpers
   src/lib/array-test.f
;SUITE

SUITE adt-option
   src/lib/adt/option-test.f
;SUITE

SUITE adt-result
   src/lib/adt/result-test.f
;SUITE

SUITE num-arithmetic
   src/lib/num-arithmetic-test.f
;SUITE

SUITE map-stdlib
   src/lib/map-test.f
;SUITE

SUITE codegen-stdlib
   src/lib/codegen-test.f
;SUITE

SUITE unicode-class-runtime
   src/lib/unicode/class-test.f
;SUITE

SUITE unicode-casefold
   src/lib/unicode-test.f
;SUITE

SUITE unicode-class-tools
   tools/unicode/class-tool-test.f
;SUITE

SUITE unicode-class-exhaustive
   tools/unicode/class-verify-main.f
;SUITE

SUITE-STDIN source-stdlib-stdin DATA
   src/lib/source-test.f -- stdin
;SUITE

SUITE argv-stdlib-mocks
   src/lib/argv-test.f
;SUITE

SUITE argv-stdlib-script-args
   src/lib/argv-test.f -- --json --label NAME --all-errors
   --strict-boundary -o OUT -- file.f --literal
;SUITE

\ Sixty-five positionals exceed the parser's 64-row table. Real argv is needed:
\ the mock table refuses the 65th append before PARSE can observe it.
SUITE argv-stdlib-capacity
   test/argv-capacity.f --
   x x x x x x x x x x x x x x x x
   x x x x x x x x x x x x x x x x
   x x x x x x x x x x x x x x x x
   x x x x x x x x x x x x x x x x x
;SUITE

SUITE test-stdlib
   src/lib/test/assert-test.f
   src/lib/test/suite-test.f
   src/lib/test/record-test.f
   src/lib/test/mapped-test.f
;SUITE

SUITE property-stdlib
   src/lib/property-test.f
;SUITE

SUITE date-helpers
   tools/stdlib-date-test.f
;SUITE

SUITE compiler-compile-floor
   test/compiler/compile-floor.f
;SUITE

\ The budgets are thread CPU time on each measurement's cheapest sample, over
\ rounds that outlast a slow stretch, and the floor means meet only a loose
\ ceiling. Measured in this pool on this 12-core machine: a scratch registry of
\ sixteen copies of this row, eight slots, beside a native build, at load
\ average 105-164: 400 of 400 rows green in 25 passes. The tools with one floor
\ round and three bench rounds were red in 2 of 128 rows in the same pool at
\ load 122-129: a whole tier-0 floor set on a slow core, and one tier-1
\ benchmark slow in all three rounds.
SUITE compiler-compile-floor-gate
   test/compile-floor-gate.f
;SUITE

SUITE icode-fixup
   test/icode-fixup-test.f
;SUITE

SUITE aot-section-reach
   tools/aot-section-reach-lint-test.f
;SUITE

SUITE aot-startup-reach
   tools/aot-startup-reach-lint-test.f
;SUITE

\ src/lib/memory-test.f is not here: its WITH-BYTES frame audit reopens package MEM
\ and reads the frame cells directly, which no shipped image has a name for.
WHITEBOX-SUITE memory
   src/lib/memory-test.f
;SUITE

SUITE tail-pure-fixtures
   src/lib/json-write-test.f
   src/lib/json-read-test.f
   src/lib/jsonl-test.f
   src/lib/decimal-test.f
   src/lib/vector-test.f
   src/lib/byte-buffer-test.f
   src/lib/elf32-test.f
   src/lib/fs-test.f
   tools/bootstrap-codegen-test.f
   tools/asm-src-test.f
   tools/image-bytes-test.f
;SUITE

\ The x86_64 seam is written and exercised from this aarch64 host: the ELF64
\ writer, the mov r64, imm64 relocation site and the x86-64 target contract.
\ Its own suite because it loads src/os/linux-x86-64/elf.f, which spells the
\ same words as the host's image writer and cannot share a process with it.
SUITE x86-64-seam
   test/x86-64-seam.f
;SUITE

\ The same seam's instruction emitters, whose bytes an aarch64 engine can produce
\ and read back. Its own suite for the same reason: it loads
\ src/os/linux-x86-64/sys.f, which spells the host's syscall-number words.
SUITE x86-64-emit
   test/x86-64-emit.f
;SUITE

\ Build executable peer fixtures through the real pass chain and ELF writer.
\ Execution on the x86-64 peer is a separate device check (docs/bootstrap.md).
SUITE x86-64-peer-image
   test/x86-64-peer-image.f
;SUITE

\ Every HIR fixture the x86-64 rows emit, one image each, the signal image that
\ raises a real signal through src/habu/boot-x64.f's handler install, the one
\ diff-negative image that proves the harness and the manifest of statuses the
\ peer must see (docs/bootstrap.md).
SUITE x86-64-peer-routines
   test/x86-64-peer-routines.f
;SUITE

\ The x86-64 boot skeleton, src/habu/boot-x64.f, and its guard-page twin, for the
\ peer to run: `hb-x64-skel a b` exits 3 and hb-x64-skel-negative exits 102.
SUITE x86-64-skel-image
   test/x86-64-skel-image.f
;SUITE

\ The x86-64 kernel's rows (src/habu/kernel-x64.f) in the booted harness,
\ test/x86-64-boot-harness.f, for the peer to run. Each file has one negative
\ image that exits 21; its header names the statuses the peer must see.
\ They load the x86-64 seam globally, as skel-image does, so each is a suite.
SUITE x86-64-kernel-syscalls
   test/x86-64-kernel-syscalls.f
;SUITE

SUITE x86-64-kernel-control
   test/x86-64-kernel-control.f
;SUITE

SUITE x86-64-kernel-atomics
   test/x86-64-kernel-atomics.f
;SUITE

SUITE x86-64-kernel-engine
   test/x86-64-kernel-engine.f
;SUITE

SUITE x86-64-kernel-ffi
   test/x86-64-kernel-ffi.f
;SUITE

SUITE x86-64-kernel-pure
   test/x86-64-kernel-pure.f
;SUITE

\ The sampling profiler's rows, src/habu/prof-x64.f, in the booted harness: its
\ header names the statuses the peer must see.
SUITE x86-64-kernel-prof
   test/x86-64-kernel-prof.f
;SUITE
\ The signal stub src/habu/boot-x64.f START, publishes, installed and raised in
\ the booted harness; its header names the statuses the peer must see.
SUITE x86-64-boot-signal
   test/x86-64-boot-signal.f
;SUITE

SUITE x86-64-kernel-task
   test/x86-64-kernel-task.f
;SUITE

\ src/habu/link-x64.f lays a captured window out over the x86-64 kernel's rows
\ and links it: the records, the routines with every site resolved, the rebased
\ wids, the protected-wid bitmap, the name index and the code cells' xts. Its
\ children measure the layout host-independent and refuse, by name, what it
\ cannot place or link. hb-x64-link-index, for the peer to run, stages the
\ writer's index where the kernel's own find reads it; it exits 0.
SUITE x86-64-link-records
   test/x86-64-link-records.f
;SUITE

\ The crash handler the x86-64 boot installs, src/habu/boot-x64.f, in the
\ booted harness: each image's child faults with fd 2 on a pipe, and its parent
\ checks the dump or the guard page's line and the exit status.
SUITE x86-64-kernel-crash
   test/x86-64-kernel-crash.f
;SUITE

\ The definition writers of the x86-64 kernel in the booted harness; the
\ header names the status each image exits with.
SUITE x86-64-kernel-definition
   test/x86-64-kernel-definition.f
;SUITE

SUITE xml-byte-edits
   src/lib/xml-test.f
   src/lib/byte-edit-test.f
   src/lib/xml-roundtrip-test.f
   src/lib/xml-source-test.f
;SUITE

SUITE xml-owner-consumer
   test/xml-owner-e2e.f
;SUITE

SUITE xml-c2-consumer
   test/c2-xml-consumer-e2e.f
;SUITE

SUITE stdlib-source-default
   src/lib/source-test.f
;SUITE

SUITE source-lex
   src/lib/source-lex-test.f
;SUITE

SUITE stdlib-process-fixtures
   tools/hb-cli-contracts-test.f
   tools/standalone-load-test.f
   test/lint-cli-standalone-load.f
   src/lib/process-test.f
   src/lib/process-command-test.f
   src/lib/process-pty-handle-test.f
;SUITE

SUITE friend-arena-seal
   test/seal.f
;SUITE

SUITE baked-owner
   test/baked-owner.f
;SUITE

SUITE internal-word-gate
   test/internal-word-gate.f
;SUITE

SUITE checker-surface
   test/checker-surface.f
;SUITE

SUITE package-seal
   test/package-seal.f
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

SUITE storage-binding
   test/storage-binding-e2e.f
;SUITE

SUITE record-launder-probe
   test/record-launder-probe.f
;SUITE

SUITE certify-dynamic-buffer
   test/certify-dynamic-buffer.f
;SUITE

SUITE c2-read-effects
   test/c2-read-effects.f
;SUITE

SUITE c2-mut-view
   test/c2-mut-view.f
;SUITE

SUITE c2-mut-records
   test/c2-mut-records.f
;SUITE

SUITE c2-loan-checker
   test/c2-loan-checker-positive.f
   test/c2-loan-checker-region.f
   test/c2-loan-checker-refusal.f
;SUITE

SUITE c2-memory-capture
   test/c2-memory-capture.f
;SUITE

SUITE c2-memory-runtime
   test/c2-memory-runtime.f
;SUITE

SUITE c2-view-record-mut
   test/c2-view-record-mut.f
;SUITE

SUITE c2-unique-policy
   test/c2-unique-policy.f
;SUITE

SUITE certify-does-definer
   test/certify-does-definer.f
;SUITE

SUITE certify-generated
   test/certify-generated.f
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

WHITEBOX-SUITE catch-stale
   test/catch-stale-suite.f
;SUITE

WHITEBOX-SUITE effect-read-api
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

SUITE outer-find
   test/outer-find.f
;SUITE

SUITE outer-interpret
   test/outer-interpret.f
;SUITE

\ main-argv reloads src/habu/main.f's source, which reopens ENGINE-MAIN; a product
\ seals every package it ships, so the row runs on the engine that keeps them open.
WHITEBOX-SUITE main-argv
   test/main-argv.f
;SUITE

SUITE engine-writers
   test/engine-writers.f
;SUITE

SUITE owner-access
   test/owner-access.f
;SUITE

SUITE checker-assert
   test/checker-assert-test.f
;SUITE

SUITE engine-span-end
   test/engine-span-end.f
;SUITE

SUITE checker-owner-descriptor
   test/checker-owner-descriptor.f
;SUITE

SUITE checker-soundness
   test/checker-soundness-suite.f
;SUITE

SUITE checker-verify-pkg-scope
   test/checker-verify-pkg-scope.f
;SUITE

SUITE checker-dup-record
   test/checker-dup-record.f
;SUITE

SUITE diag-buffer-capacity
   test/diag-buffer-capacity.f
;SUITE

WHITEBOX-SUITE checker-verify-order
   test/checker-verify-order.f
;SUITE

WHITEBOX-SUITE checker-decl-locs
   test/checker-decl-locs.f
;SUITE

WHITEBOX-SUITE checker-completion
   test/checker-completion.f
;SUITE

SUITE checker-decl-locs-capture
   test/checker-decl-locs-capture.f
;SUITE

WHITEBOX-SUITE checker-replay-pkg-state
   test/checker-replay-pkg-state.f
;SUITE

SUITE verify-prim
   test/verify-prim-test.f
;SUITE

SUITE defer-history
   test/defer-history.f
;SUITE

SUITE parses-window
   test/parses-window.f
;SUITE

WHITEBOX-SUITE effect-intern
   test/effect-intern-suite.f
;SUITE

WHITEBOX-SUITE effect-store-census
   test/effect-store-census-test.f
;SUITE

WHITEBOX-SUITE checker-dead-path
   test/checker-dead-path-suite.f
;SUITE

SUITE local-spelling
   test/local-spelling-suite.f
;SUITE

WHITEBOX-SUITE checker-rollback-sig-pool
   test/checker-rollback-sig-pool.f
;SUITE

SUITE snapshot-writer
   test/snapshot-writer.f
;SUITE

SUITE stdlib-standalone-load
   test/stdlib-standalone-load.f
;SUITE

SUITE aot-wid-restore
   test/aot-wid-suite.f -- restore
;SUITE

SUITE aot-wid-refuse-wid0
   test/aot-wid-suite.f -- refuse-wid0
;SUITE

SUITE aot-wid-refuse-address-span
   test/aot-wid-suite.f -- refuse-address-span
;SUITE

SUITE aot-wid-boot-sealed
   test/aot-wid-suite.f -- boot-sealed
;SUITE

SUITE aot-wid-rebase
   test/aot-wid-suite.f -- rebase
;SUITE

SUITE aot-wid-capture-refusal
   test/aot-wid-suite.f -- capture-refusal
;SUITE

SUITE aot-data-window
   test/aot-data-window-suite.f
;SUITE

SUITE aot-seed-batch
   test/aot-seed-batch-suite.f
   test/aot-seed-metadata.f
;SUITE

SUITE aot-capture-compact
   test/aot-capture-compact.f
;SUITE

\ A capture taken with an x86-64 shadow open carries the shadow's records,
\ routines, sites and record-keyed code cells through the artifact's round trip;
\ READ refuses forged shadow rows by name and MERGE refuses a shadow.
SUITE aot-shadow-capture
   test/aot-shadow-capture.f
;SUITE

SUITE data-address-codec
   test/data-address-codec.f
   tools/snap-heap-owner-test.f
;SUITE

SUITE aot-named-cells
   test/aot-named-cells.f
;SUITE

SUITE aot-named-cells-native
   test/aot-named-cells.f -- native
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

SUITE aot-cell-values
   test/aot-cell-values.f
;SUITE

SUITE aot-registry-identity
   test/aot-registry-identity.f
;SUITE

SUITE aot-payload-graph
   test/aot-payload-graph.f
;SUITE

SUITE checker-graph-domains
   test/checker-graph-domains.f
;SUITE

SUITE aot-prefix-literal
   test/aot-prefix-literal.f
;SUITE

SUITE aot-payload-unsupported
   test/aot-payload-unsupported.f
;SUITE

SUITE aot-sig-pool
   test/aot-sig-pool-suite.f
;SUITE

SUITE aot-effect-pool
   test/aot-effect-pool.f
;SUITE

SUITE heap-start-cell
   test/heap-start-cell.f
;SUITE

SUITE signal-stub
   test/signal-stub.f
;SUITE

SUITE compiler-native-create-does
   test/compiler/native-create-does.f
;SUITE

SUITE tier
   test/tier.f
;SUITE
SUITE does-clause-record
   test/does-clause-record.f
;SUITE

SUITE does-empty-clause
   test/does-empty-clause.f
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

WHITEBOX-SUITE snapshot-xt-cell-decl
   test/snapshot-xt-cell-decl.f
;SUITE

SUITE address-cell-rollback
   test/address-cell-rollback.f
;SUITE

SUITE address-cell-rollback-aot
   src/lib/test.f
   test/compiler/aot-mode.f
   ENTRIES
   test/address-cell-rollback.f
;SUITE

SUITE address-cell-cap-grown
   test/address-cell-cap-grown.f
;SUITE

SUITE address-cell-index
   test/address-cell-index.f
;SUITE

SUITE address-cell-index-recovery
   test/address-cell-index-recovery.f
;SUITE

SUITE address-cell-storage-oom
   test/address-cell-storage-oom.f
;SUITE

SUITE address-cell-tasks
   test/address-cell-tasks.f
;SUITE

SUITE catch-frame
   test/catch-frame.f
;SUITE

SUITE engine-stack-wide
   test/engine-stack-wide.f
;SUITE

SUITE engine-stack-lifecycle
   test/engine-stack-lifecycle.f
;SUITE

SUITE stack-guard
   test/stack-guard.f
;SUITE

SUITE engine-stack-jit
   test/engine-stack-jit.f
;SUITE

SUITE engine-stack-machine
   test/engine-stack-machine.f
;SUITE

SUITE engine-stack-debugger
   test/engine-stack-debugger.f
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

\ The design seal: each test/policy case loaded sealed at both tiers, one child
\ per row; writes build/policy-run.txt.
SUITE policy
   src/lib/policy-test.f
;SUITE

SUITE trust-row-refusal
   test/trust-row-test.f
;SUITE

SUITE shadowed-arity-refusal
   test/shadowed-arity-test.f
;SUITE

SUITE load-reject-diag
   test/load-reject-diag-test.f
;SUITE

SUITE core-prefix-mark
   test/prefix-mark-test.f
;SUITE

SUITE stdlib-runner-fixtures
   src/lib/test/runner-test.f
;SUITE

SUITE stdlib-build-fixtures
   src/lib/build-test.f
;SUITE

SUITE load-argv-contract
   tools/load-argv-test.f
;SUITE

SUITE cold-argv-separator
   test/cold-argv-separator.f
;SUITE

SUITE build-rewind
   test/build-rewind-test.f
;SUITE

SUITE cold-runtime
   test/cold-runtime-test.f
;SUITE

SUITE cold-naming
   test/cold-naming-test.f
;SUITE

SUITE tmp-path
   test/tmp-path-test.f
;SUITE

SUITE gate-pool
   test/gate-pool-test.f
;SUITE

SUITE gate-images-orchestration
   test/gate-images-test.f
;SUITE

SUITE gate-common
   test/gate-common-test.f
;SUITE

SUITE num-types
   src/lib/num-types-test.f
;SUITE

SUITE span
   src/lib/span-test.f
;SUITE

SUITE fs-mutate
   src/lib/fs-mutate-test.f
   src/lib/fs-rename-noreplace-test.f
;SUITE

SUITE exit-hook
   test/exit-hook-test.f
;SUITE

SUITE fs-copy-alias
   src/lib/fs-copy-alias-test.f
   src/lib/fs-identity-test.f
;SUITE

SUITE fs-lock
   src/lib/fs-lock-test.f
;SUITE

SUITE fs-list
   src/lib/fs-list-test.f
;SUITE

SUITE tree-copy
   src/lib/tree-copy-test.f
;SUITE

SUITE pty
   src/lib/pty-test.f
;SUITE

\ Raw serial streams over a pty pair: the loop supplies every wait.
SUITE serial
   src/lib/serial-test.f
;SUITE

\ The pty test harness's buffer: the compaction, the span search and the
\ never-seen facts, driven without a child; plus the two cases that need one,
\ the spawn's abort path and the bounded reap of a wedged child.
SUITE pty-harness
   src/lib/pty-harness-test.f
;SUITE

\ IPv4 stream sockets: a listener task and a client in one process.
SUITE tcp4
   src/lib/net/tcp4-test.f
;SUITE

\ IPv4 datagrams: two loopback sockets in one process, every wait bounded.
SUITE udp4
   src/lib/net/udp4-test.f
;SUITE

\ The RFC 6455 frame codec: bytes in and bytes out, no socket.
SUITE websocket-frame
   src/lib/net/ws-frame-test.f
;SUITE

\ SSE framing over arbitrary byte chunks, with no network dependency.
SUITE event-stream
   src/lib/net/event-stream-test.f
;SUITE

\ RFC 6455 connections on the HTTP server, spoken to by a Habu client over
\ TCP4; writes build/ws-transcript.txt and compares it whole.
SUITE websocket
   src/lib/net/ws-test.f
;SUITE

\ The process row is per task: a capturing task and a polling task at once.
SUITE process-tasks
   src/lib/process-task-test.f
;SUITE

\ Generic text I/O devices: the engine's own text through a memory device, and
\ a REPL over a loopback connection.
SUITE genio
   src/lib/genio-test.f
;SUITE

SUITE process-argv
   src/lib/process-argv-test.f
;SUITE

SUITE process-cwd
   src/lib/process-cwd-test.f
;SUITE

SUITE process-env
   src/lib/process-env-test.f
;SUITE

\ A tree walk the kernel refuses throws rather than reading the refusal as
\ nobody there, and one at a capture's early end leaves the capture its own
\ answer and names the walk's code.
SUITE process-tree
   src/lib/process-tree-test.f
;SUITE

SUITE test-subject
   src/lib/test/subject-test.f
;SUITE

SUITE test-eval
   src/lib/test/eval-test.f
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

SUITE whitebox-engine-key
   test/whitebox-engine-key-test.f
;SUITE

SUITE fixture-cache
   test/fixture-cache-test.f
;SUITE

SUITE keyed-image-reap
   test/keyed-image-reap-test.f
;SUITE

SUITE build-cache-retain
   src/lib/build-cache-retain-test.f
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

SUITE deep-cwd
   test/deep-cwd-e2e.f
;SUITE

SUITE include-refusal
   test/include-refusal-e2e.f
;SUITE

SUITE whole-source-read
   test/whole-source-read-test.f
;SUITE

SUITE room-left
   test/room-left-test.f
;SUITE

SUITE name-length
   test/name-length-test.f
;SUITE

SUITE boot-row
   test/boot-row-test.f
;SUITE

SUITE boot-relocation
   test/boot-relocation-e2e.f
;SUITE

SUITE json
   tools/json-test.f
;SUITE

SUITE source-discovery
   tools/source-discovery-test.f
;SUITE

SUITE xref
   tools/xref-test.f
   test/native-unit-xref-newest.f
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

SUITE linear
   test/linear-suite.f
;SUITE

WHITEBOX-SUITE decl-event
   test/decl-event-suite.f
;SUITE

SUITE deftype
   test/deftype-suite.f
;SUITE

SUITE closed-source
   test/closed-source-suite.f
;SUITE

WHITEBOX-SUITE closed-unit
   test/closed-unit-suite.f
;SUITE

WHITEBOX-SUITE engine
   test/engine-suite.f
;SUITE

SUITE debugger-resume
   test/debugger-resume.f
;SUITE

SUITE engine-runtime-regressions
   test/runtime-regression-test.f
;SUITE

WHITEBOX-SUITE declaration-replay-source
   test/decl-replay-verify-source.f
;SUITE

SUITE program-diagnostics
   test/program-diagnostics-test.f
;SUITE

SUITE diag-json-escape
   test/diag-json-escape.f
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

SUITE profiler-index
   test/prof-index.f
;SUITE

SUITE perf-map-fixtures
   tools/perf-map-test.f
;SUITE

SUITE native-gate-dictionary
   test/gate-dictionary.f
;SUITE

SUITE dictionary-capacity
   test/dictionary-capacity-e2e.f
;SUITE

SUITE proc-maps
   test/proc-maps.f
;SUITE

SUITE type-layout-lower-pending
   test/type-layout-lower-pending.f
;SUITE

\ A wide layout value — arity 0 and a parametric instance — inside a typed
\ local, run at both tiers: the annotation is a checker rule, the whole-bundle
\ reload is code the native compiler emits, so tier 1 is a separate run and not
\ a repeat.
SUITE wide-typed-local-probe
   test/wide-typed-local-probe.f
;SUITE

SUITE wide-typed-local-probe-aot
   src/lib/test.f
   test/compiler/aot-mode.f
   ENTRIES
   test/wide-typed-local-probe.f
;SUITE

SUITE layout-buffer
   test/layout-buffer.f
;SUITE

SUITE dynamic-buffer
   test/dynamic-buffer.f
;SUITE

SUITE values
   test/values.f
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

SUITE aot-capture-bound
   test/aot-capture-bound.f
;SUITE

SUITE dynamic-buffer-tasks
   test/dynamic-buffer-tasks.f
;SUITE

SUITE layout-defer
   test/layout-defer.f
;SUITE

WHITEBOX-SUITE lower-cert
   test/lower-cert.f
;SUITE

SUITE layout-buffer-depth
   test/layout-buffer-depth.f
;SUITE

WHITEBOX-SUITE layout-valid-guards
   test/layout-valid-guards.f
;SUITE

WHITEBOX-SUITE layout-valid-growth
   test/layout-valid-growth.f
;SUITE

SUITE wide-store-seal
   test/wide-store-seal.f
;SUITE

SUITE protection-span
   test/protection-span.f
;SUITE

SUITE data-claims-build
   test/data-claims-build.f
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

SUITE sites
   test/sites.f
;SUITE

SUITE p2-map-rewind
   test/p2-map-rewind.f
;SUITE

SUITE lower-txn-large
   test/lower-txn-large.f
;SUITE

WHITEBOX-SUITE bootstrap-wide-memory-src
   test/bootstrap-wide-memory-src.f
;SUITE

WHITEBOX-SUITE enum-decl
   test/enum-decl-suite.f
;SUITE

SUITE extent-product
   test/extent-product-test.f
;SUITE

WHITEBOX-SUITE field-proj
   test/field-proj-suite.f
;SUITE

WHITEBOX-SUITE field-proj-errors
   test/field-proj-errors.f
;SUITE

SUITE compiler-native-field-proj
   test/compiler/native-field-proj.f
;SUITE

SUITE gate-pool-orphan
   test/gate-pool-orphan-test.f
;SUITE

\ A signalled gate root, and a row past its deadline, leave no process and no
\ scratch behind.
SUITE gate-signal
   test/gate-signal-test.f
;SUITE

\ A signalled check.f, and a check run past its deadline, leave no process and
\ no scratch behind.
SUITE check-signal
   test/check-signal-test.f
;SUITE

\ A capture that ends its child early - its deadline, an overflow, a refused
\ reaper arm - leaves nothing the child started.
SUITE capture-tree
   test/capture-tree-test.f
;SUITE

WHITEBOX-SUITE generated-declaration-transaction
   test/generated-declaration-transaction-suite.f
;SUITE

SUITE golden
   test/golden-test.f
;SUITE

SUITE lit-emit-size
   test/lit-emit-size-test.f
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

SUITE structure-quotation-field
   test/structure-quotation-image.f
;SUITE

SUITE deferred-quotation-image
   test/deferred-quotation-image.f
;SUITE

SUITE structure-quotation-rollback
   test/structure-quotation-rollback.f
;SUITE

WHITEBOX-SUITE structure-decl
   test/structure-decl-suite.f
;SUITE

SUITE structure-opaque
   test/structure-opaque-e2e.f
;SUITE

WHITEBOX-SUITE structure-make
   test/structure-make-suite.f
;SUITE

WHITEBOX-SUITE type-ctor
   test/type-ctor-suite.f
;SUITE

WHITEBOX-SUITE type-decl
   test/type-decl-suite.f
;SUITE

SUITE declaration-headers
   test/declaration-headers-e2e.f
;SUITE

WHITEBOX-SUITE type-export
   test/type-export-suite.f
;SUITE

WHITEBOX-SUITE type-family-rollback
   test/type-family-rollback-suite.f
;SUITE

WHITEBOX-SUITE type-family
   test/type-family-suite.f
;SUITE

\ The init test reads sealed type-family and checker metadata to verify its
\ instantiated physical layout; the refusal cases still use real source loads.
WHITEBOX-SUITE c2-init-schema
   test/c2-init-schema.f
;SUITE

WHITEBOX-SUITE c2-init-record-schema
   test/c2-init-record-schema.f
;SUITE

\ The checker's rules for code written inside C2-MEM, on the engine that keeps
\ engine packages open; a product seals C2-MEM, which c2-memory pins.
WHITEBOX-SUITE c2-reopen-refusals
   test/c2-reopen-refusals.f
;SUITE

\ C2-MEM's own view packs and unpacks compiled at tier 1 in the reopened package.
WHITEBOX-SUITE c2-view-cast
   test/c2-view-cast.f
;SUITE

SUITE c2-init-accessors
   test/c2-init-accessor-refusals.f
;SUITE

SUITE c2-init-accessor-private
   test/c2-init-accessor-private.f
;SUITE

SUITE c2-init-accessor-image
   test/c2-init-accessor-e2e.f
;SUITE

SUITE c2-records
   test/c2-records-e2e.f
;SUITE

SUITE c2-diagnostics
   test/c2-diagnostics.f
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

\ The sequential group comes last. Entering it drains the pool (GROUP-HEADER in
\ src/lib/test/suite.f), so anywhere earlier every slot idles until the rows before
\ it finish, and the rows after it start only when it ends.
GROUP SEQ native-serial-gates

\ The PTY REPL fixture starts and reaps eight engine children. Keep it in the
\ idle serial group so the fixed 20 s child-reap budget is not consumed by a
\ saturated suite pool.
SUITE repl-address-cell-rollback
   test/repl-address-cell-rollback.f
;SUITE

\ The open-definition PTY fixture starts and reaps four engine children, under
\ the same fixed reap budget.
SUITE repl-open-definition
   test/repl-open-definition.f
;SUITE

SUITE repl-literal-segment
   test/repl-literal-segment.f
;SUITE

SUITE native-gate-diagnostics
   test/gate-diagnostics.f
;SUITE

;GROUP

RUN

;using

s" PASS: native tests" type cr
