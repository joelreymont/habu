# FILEMAP.md

Fast orientation for coding agents when code search is unavailable or expensive.
`tools/filemap-lint.f` keeps these paths live and checks that required entry
points stay listed.

## Agent Context

- `AGENTS.md` — repository conventions, workflow, and gate.
- `.dots/` — active implementation tasks; `dot ready` shows unblocked work.
- `LLM.md` — operating protocol for LLM-written Forth.
- `LESSONS.md` — running project memory and recent implementation findings.
- `STATUS.md` — current verification status and known gaps.
- `TRUSTED.md` — audited `TRUST` escape-hatch manifest.
- `skills/habu-bootstrap/SKILL.md` — current bootstrap and refresh commands.
- `skills/habu-gate/SKILL.md` — current native gate commands and timing args.
- `skills/habu-host-profiles/SKILL.md` — host-class macOS and Jetson/Orin timing
  profiles.
- `skills/habu-build/SKILL.md` — current AOT and REPL build commands.
- `docs/bootstrap.md` — no-binary recovery, native refresh, and porting.
- `docs/forth.md` — blocking Forth style rules.
- `docs/type-families.md` — generic lowercase type-family/ADT design plan, including the canonical shared field key, transaction lifecycle, persistence, and reflection contract.
- `docs/effects.md` — stack-effect syntax plus the R8 CAD capability-effect design (static op-schema row, checker propagation, Maki registration, capability tokens, planner legality, runtime resolver, cache identity).
- `docs/registry-band.md` — write-protecting the type-registry control cells: Layer 1 (shipped) marks the din=0 registry cells internal, closing bare naming and direct checked references; the analysis that a PROT-GUARD memory band does NOT close the laundered `[']`→execute route (its bypass writer is itself launderable) and that the root cause/fix is the checker gap `habu-checker-exec-of-5923c543`; the TDECL snapshot redundancy; and the per-sibling rollout recipe.
- `docs/census-switchover.md` — site-level inventory for the post-TFAM switchover: sentinel-return conventions to migrate to option/result, legacy enum clusters, value-record/PTX-IR products, ADT-dischargeable trust rows, and the wave-ordered migration plan.
- `docs/gate.md` — native gate architecture, proof subjects, metrics, and
  process-boundary rules.
- `docs/habu-pitch.md` — the research pitch: eliminate silently-wrong GPU kernels (the Triton #10927 K=511 class) by writing the math (`O[m,n] = Σk A[ix[m],k] · B[n,k]`) and letting the compiler derive tiling, fusion, data movement, precision, and the backward pass, or refuse to compile.
- `docs/kernel-principles.md` — roofline, the 3 bounds, the device's compute/memory roofs, and where each Habu kernel sits (apply before optimizing any kernel).
- `docs/parallel-agents.md` — map-reduce protocol for parallel dot execution.
- `docs/ptx.md` — Habu→PTX GPU DSL strategy and scope.
- `docs/ptx-sketch.md` — Habu→PTX v0 language spec.
- `docs/inference.md` — Habu→PTX local type inference: infer bodies, annotate the contract edge.
- `docs/autograd.md` — Habu→PTX reverse-mode autograd: AD as a syntactic reversal; verified gradients.
- `docs/eval-triton.md` — eval matrix: checked Habu-PTX vs real Triton on the Orin (install, reproduction, results).
- `docs/case-tma-stride.md` — Triton #10927 case: silent TMA stride-misalignment corruption; the motivating design-rule case.
- `docs/tma-gather.md` — gathered GEMM + TMA movement-plan design: planner-owned gather lowerings, emitter surface, legality rules, sm_121a target.
- `docs/golden-syntax.md` — golden-authoring syntax exploration: extent-typed tensor accessors and the spec word.
- `docs/extent-substrate.md` — decision record: EXTENT:/idx<#M> mints on TFAM families (not CT roles or extent-atoms); criterion matrix, rejected-substrate costs, A1b/extent-tensor re-scope drafts.
- `docs/seed.md` — native seed trust root and no-binary recovery.
- `docs/typed-top-level.md` — checker-modeled typed top level design (top-row
  tracker, xt<effect> typing, adoption tiers) plus the parametric-cell V2
  verdict.
- `docs/swiftforth-task-api.md` — SwiftForth multitasking surface captured for
  Habu `TASK` package parity.

## Core Checker

- `src/core/util.f` — shared subset helpers.
- `src/core/cell.f` — target cell-width constant and load-time native/recovery invariant.
- `src/core/cell-effects.f` — post-hook checked effects for target cell-width words.
- `src/core/pointer-storage.f` — one-concern `PTR-VARIABLE` pointer-cell definer.
- `src/core/pointer-storage-effects.f` — post-hook checked effect for `PTR-VARIABLE`.
- `src/core/bytes.f` — core byte-buffer helpers (`BYTE+`, `BYTE-COPY-LEN`,
  `BYTE-COPY`) loaded before stdlib/tool sources so low-level modules do not
  depend on `lib/string.f` order.
- `src/core/engine-error.f` — authoritative package-scoped engine failure ABI.
- `src/core/engine-error-effects.f` — checker rows installed after the early engine failure package.
- `src/core/checker.f` — native stack-effect checker and verifier.
- `src/core/lower-cert-base.f` — boot-safe, package-scoped lowering-certificate ABI and fail-closed producer dispatcher loaded immediately after the checker.
- `src/core/type-schema.f` — persistent type-schema node arena (package TFAM) referenced by families/variants/fields as schema roots.
- `src/core/type-family.f` — package-scoped TFAM/SUMV/layout registries and the shared transactional field-schema arena with canonical STACK/PACKED validation, sealed `TYPE-FIELD` reflection, and snapshot/rollback integration.
- `src/core/render.f` — human/JSON diagnostics and signature recording.
- `src/core/sumtype.f` — TYPEFAMILY/SUMTYPE/PRODUCT declaration grammar registering package-aware families, variants, and atomic shared field schemas.
- `src/core/layout-buffer.f` — generative checked storage for closed ADT layouts; owns allocation, zero initialization, bounds, stride, and the sole typed-layout pointer introduction boundary.
- `src/core/layout-valid.f` — package-scoped producer for immutable, source-bound lowering certificates: canonical source-offset width rows, bind widths, fetch descriptors, and exact guard-domain evidence.
- `src/core/layout-buffer-seal.f` — post-xref capability erasure for layout-buffer authorization.
- `src/core/lower-cert-seal.f` — post-xref capability erasure for lowering-certificate producer hooks and backing authority cells.
- `src/core/roles.f` — audited nominal scalar role conversion words.
- `src/core/include.f` — checked source composition words (`include`, `included`) with dynamic `evaluate` isolated to `INCLUDE-EVALUATE`, plus the ordered source-composition event log (`EVENT-RECORD`, `EVENT-ON`/`DISCOVERY-ON`) that records include multiplicity and require/provided registry state, and `REQUIRE-SNAPSHOT`/`REQUIRE-RESTORE` giving the discovery pass a fresh require registry without disturbing warm-snapshot state.
- `src/core/structures.f` — post-hook checked `BEGIN-STRUCTURE`, `+FIELD`, `CFIELD:`, and `END-STRUCTURE` layout DSL definitions pending hard deletion.
- `src/core/structures-effects.f` — retired pre-hook effect rows retained only as hard-deletion input; no boot path loads them.
- `src/core/enums.f` — checked `ENUM+` and `ENUM4+` legacy numeric counter definers for named integer families.
- `src/core/exec-vector.f` — checked execution-vector support for `defer`/`is` runtime sentinels.
- `src/core/sha256.f` — standalone SHA-256, streaming file digest, and hex helpers.
- `src/core/type-family-sha.f` — installs the constructor package-name SHA-256 fallback hook (TF-SHA16) after sha256.f loads.
- `src/core/sha-check.f` — standalone SHA-256 self-test against FIPS-180 vectors.
- `src/core/check-hook.f` — default native source checker hook installation.
- `src/core/top-row.f` — tier-1 top-level row tracker: installs the per-token
  top-row hook as the last cold-prefix source and warns (stderr, rc unchanged)
  on the xt/value residuals a depth floor cannot reach (docs/typed-top-level.md
  §5 sub-dot 3).
- `src/core/internal-mark.f` — seal-time internal-word marking pass (last
  cold-prefix source): sets `DNAME-INT` on every engine-prefix COLON record
  with no checker-known effect so bare top-level execution and tick fail
  closed (`hb: internal engine word:`, rc 70); data records are exempt
  (push-only, engine-auto-trusted class); self-sealing unchecked boundary.
- `src/core/combinators.f` — legacy higher-order library words baked into
  `bin/hb` (audited unchecked boundary; new higher-order words are checked).

## Native Engine And Builders

- `src/habu/habu1.f` — primitive engine, dictionary, parser, and prim registry.
- `src/habu/habu2.f` — compiler/control-flow layer.
- `src/habu/driver-io.f` — shared fail-closed artifact writer for internal
  build drivers.
- `src/habu/engine-size.f` — exact emitted-engine region measurement rows.
- `src/habu/jit.f` — register JIT helpers.
- `src/habu/regalloc.f` — virtual stack register allocator.
- `src/habu/aot-capture.f` — host-only AOT-REPL capture: scans metabuild-compiled words for inter-word call sites and builds the seed blob/records/reloc-table.
- `src/habu/aot-closure.f` — stripped AOT closure analysis and diagnostics.
- `src/habu/aot-lib.f` — stripped AOT linker library.
- `src/habu/aot.f` — stripped AOT maker entry.
- `src/habu/build.f` — `hb-build --repl` bundle driver.
- `src/habu/maker.f` — generic maker-image build driver for `hb-build`.
- `src/habu/snap-lib.f` — checked snapshot writer definitions.
- `src/habu/snap.f` — snapshot writer entry point.
- `src/habu/stdin.f` — internal stdin/interactive engine builder.
- `src/habu/rt.f` — native runtime routines emitted for the engine builder
  (stack, dictionary, and interpreter support).
- `src/habu/repl.f` — interactive REPL baked into the stdin engine.
- `src/habu/crash.f` — in-binary crash handler printing the register dump.
- `src/habu/prof.f` — in-binary sampling profiler (`prof-on`/`prof-report`).
- `src/habu/stage2.f` — fixpoint driver: the running stage1 engine rebuilds the
  next engine from current source.
- `src/habu/treeshake.f` — build-time primitive tree shaker for `hb-build`
  makers.
- `src/habu/verify-source.f` — pre-compile checked source verifier.
- `src/habu/bundle-argv.f` — standalone bundle script argument convention.
- `src/arch/arm64/asm.f` / `src/arch/arm64/icode.f` / `src/arch/arm64/mnem.f`
  — standalone ARM64 instruction encoders, the minimal single-pass assembler,
  and the icode-style mnemonic layer over the encoders.
- `src/os/env-base.f` — shared startup argv/envp access over captured DATA
  cells.
- `src/os/script-argv.f` — `bin/hb` source-list script argument convention.
- `src/os/linux/sys.f` / `src/os/macos/sys.f` — per-target OS seams: syscall
  numbers plus the SVC emitter.
- `src/os/linux/target.f` / `src/os/macos/target.f` — runtime/build-script
  target flag words.

## Gforth Bootstrap Recovery

- `bootstrap/habu.fs` — top-level Gforth no-binary recovery driver.
- `bootstrap/habu-lib.fs` — shared Gforth bootstrap library bundle.
- `bootstrap/habu-cg.fs` — Gforth codegen bootstrap bundle.
- `bootstrap/habu-repl.fs` — Gforth REPL bootstrap entry.
- `bootstrap/habu-tui.fs` — Gforth TUI bootstrap entry.
- `bootstrap/examples.fs` — bootstrap example source.
- `bootstrap/src/arena.fs` — bootstrap arena allocator.
- `bootstrap/src/capture.fs` — bootstrap source capture.
- `bootstrap/src/checker.fs` — bootstrap checker entry.
- `bootstrap/src/colon.fs` — bootstrap colon-definition parser.
- `bootstrap/src/config.fs` — bootstrap configuration constants.
- `bootstrap/src/control.fs` — bootstrap control-flow checker.
- `bootstrap/src/db.fs` — bootstrap dictionary storage.
- `bootstrap/src/defining.fs` — bootstrap defining words.
- `bootstrap/src/diag-state.fs` — bootstrap diagnostic state.
- `bootstrap/src/diag.fs` — bootstrap diagnostic rendering.
- `bootstrap/src/effects-repr.fs` — bootstrap effect representation.
- `bootstrap/src/forward.fs` — bootstrap forward-declaration handling.
- `bootstrap/src/habu.fs` — bootstrap engine assembly.
- `bootstrap/src/locals.fs` — bootstrap locals checker.
- `bootstrap/src/parsing.fs` — bootstrap parser helpers.
- `bootstrap/src/pickroll.fs` — bootstrap stack primitive helpers.
- `bootstrap/src/prims.fs` — bootstrap primitive signatures.
- `bootstrap/src/quots.fs` — bootstrap quotation support.
- `bootstrap/src/render.fs` — bootstrap effect renderer.
- `bootstrap/src/repl.fs` — bootstrap REPL support.
- `bootstrap/src/rows.fs` — bootstrap row operations.
- `bootstrap/src/runtime.fs` — bootstrap runtime support.
- `bootstrap/src/sig.fs` — bootstrap signature model.
- `bootstrap/src/sigparse.fs` — bootstrap signature parser.
- `bootstrap/src/tui.fs` — bootstrap terminal UI support.
- `bootstrap/src/types.fs` — bootstrap type model.
- `bootstrap/src/unify.fs` — bootstrap unifier.
- `bootstrap/cg/asm.fs` — Gforth ARM64 assembler helpers.
- `bootstrap/cg/asm-checked.fs` — Gforth checked assembler surface.
- `bootstrap/cg/cglocals.fs` — Gforth locals codegen.
- `bootstrap/cg/cgloop.fs` — Gforth loop codegen.
- `bootstrap/cg/cgquot.fs` — Gforth quotation codegen.
- `bootstrap/cg/crash.fs` — Gforth crash handler codegen.
- `bootstrap/cg/disasm-core.fs` — Gforth disassembler core.
- `bootstrap/cg/disasm.fs` — Gforth disassembler entry.
- `bootstrap/cg/elf.fs` — Gforth ELF image writer.
- `bootstrap/cg/exec.fs` — Gforth executable emitter.
- `bootstrap/cg/forth.fs` — Gforth stage0 engine code generator.
- `bootstrap/cg/icode.fs` — Gforth instruction-code layer.
- `bootstrap/cg/image.fs` — Gforth image layout writer.
- `bootstrap/cg/inspect.fs` — Gforth inspection tools.
- `bootstrap/cg/install.fs` — Gforth install helpers.
- `bootstrap/cg/jit.fs` — Gforth JIT emitter mirror.
- `bootstrap/cg/link.fs` — Gforth link helpers.
- `bootstrap/cg/macho.fs` — Gforth Mach-O image writer.
- `bootstrap/cg/opt.fs` — Gforth peephole optimizer.
- `bootstrap/cg/prof.fs` — Gforth profiler support.
- `bootstrap/cg/regalloc.fs` — Gforth register allocator mirror.
- `bootstrap/cg/regstack.fs` — Gforth virtual stack register model.
- `bootstrap/cg/rt.fs` — Gforth runtime emitter.
- `bootstrap/cg/sha256.fs` — Gforth SHA-256 helper.
- `bootstrap/cg/sign.fs` — Gforth signing helper.
- `bootstrap/cg/stepper.fs` — Gforth stepper support.
- `bootstrap/cg/sys.fs` — Gforth OS syscall helpers.
- `bootstrap/cg/templ.fs` — Gforth template emitter.
- `bootstrap/cg/walk.fs` — Gforth source walker.

## Debugging And Inspection

- `docs/debugging.md` — first stop for runtime/codegen RCA tooling; covers the
  baked REPL stepper, breakpoint debugger, watch cells, stack inspection, and
  native disassembly/image inspection commands.
- `src/habu/debug-watch.f` — REPL watch table for cells printed by both `step`
  and breakpoint traps.
- `src/habu/stepper.f` — baked REPL `step` implementation that runs one token at
  a time and prints stack/watch state.
- `src/habu/debug.f` — baked REPL compiled-word breakpoint commands:
  `BP+`, `BP*`, `BPN`, `BP-`, and `BP.`.
- `src/habu/layout.f` — shared native image, dictionary, and snapshot layout
  constants used by builders and image inspection tools.
- `src/habu/hide.f` — refresh-only dictionary/signature truncation prelude used
  before reloading common engine source.
- `src/habu/xref.f` — baked live dictionary lifecycle/inspection words:
  `undefine`, `LATEST`, `XREF-FIND`, `XREF.`, `XREF`, `SEE`, and `WORDS`.
- `src/arch/ptx/emit.f` — checked PTX text encoder for the sm_87 SAXPY M3
  toolchain spike.
- `src/arch/ptx/vjp.f` / `src/arch/ptx/vjp-test.f` — the `VJP:` paired-word
  table for the M6 forward primitives (adjoint expansion + saves count per
  entry, consumed by lib/ptx/ad.f) plus per-entry unit tests including the
  review-corrected OVER fan-out-sum and DROP typed-zero direction facts.
- `lib/ptx/test-prelude.f` — require-only shared setup for PTX positive entry
  tests; suites list the entry tests, not this dependency bundle.
- `lib/ptx/process-test-prelude.f` — require-only process-boundary setup for
  PTX tests that must exercise a child process boundary.
- `lib/ptx/toolchain.f` — checked PTX private artifact root plus `PTXAS`
  resolution and assembler runner for device tests.
- `lib/ptx/toolchain-test.f` — checked fixture coverage for PTX artifact
  uniqueness, cleanup, and assembler path resolution.
- `lib/ptx/sentinel.f` — device-readback poison sentinel (`FILL` pre-launch,
  `GUARD` fail-closed) so a dropped copy-back cannot masquerade as a passing golden.
- `lib/ptx/sentinel-test.f` — checked coverage for the readback sentinel fill,
  pass-through, and fail-closed throw.
- `lib/ptx/cuda-driver.f` — canonical checked CUDA Driver API (package CUDA):
  nominal cuda-dev/ctx/mod/fn/devptr handle roles, hyphenated FFI bindings
  (`CU-INIT`, `CU-DEVICE-GET`, ...), fail-closed `CUDA-HANDLE0`/`CUDA-RC0`
  guards (`E-CUDA`), and typed helpers (`LOAD-MODULE`, `GET-FUNCTION`,
  `DEVICE-ALLOC`, `HTOD`, `DTOH`). maki and tools/ptx share this one resolver.
- `lib/ptx/cuda-driver-test.f` — portable CUDA Driver binding and fail-closed
  guard regressions (null handle and nonzero CUresult throw `E-CUDA`).
- `lib/ptx/neg-test-lib.f` — require-only in-process helper for PTX semantic
  rejection tests that call the checker directly and capture diagnostics.
- `lib/ptx/launch.f` — checked PTX launch-contract helpers for row kernels
  (`rows > 0`, `cols > 0`, legal block, and `cols <= block`).
- `lib/ptx/launch-test.f` — checked fixtures for PTX launch-contract rejection.
- `lib/ptx/ir.f` — checked value-numbered PTX expression IR with constant
  folding, peephole canonicalization, CSE, and DCE live marking.
- `lib/ptx/ir-test.f` — checked value fixtures for PTX IR fold, peephole,
  CSE, DCE, and overflow rejection.
- `lib/ptx/opt-ir.f` — line-oriented instruction-table IR over emitted PTX
  text: parses body lines into typed pure/opaque records and re-renders them.
- `lib/ptx/opt.f` — sound bit-exact optimization passes over that IR
  (copy-prop, constant-fold, CSE, DCE, self-move peephole); opt-in OFF by
  default via PTX-MAYBE-OPT.
- `lib/ptx/opt-ir-test.f` — checked fixtures for opt-ir line classification,
  operand parsing, fail-closed passthrough, and byte-exact round-trip.
- `lib/ptx/opt-test.f` — per-pass before/after fixtures, idempotence,
  fma-refusal, and safety (saxpy/gelu/cg-mma semantics + count deltas).
- `lib/ptx/ad-dag.f` — checked reverse-mode symbolic DAG builder for PTX row
  kernels (softmax primitives plus row×row `*.`/`+.`).
- `lib/ptx/ad-dag-test.f` — checked validation tests for PTX AD DAG overflow,
  underflow, unknown opcode, non-singleton output, and `*.`/`+.` build/arity.
- `lib/ptx/ad-dag-eval.f` — checked HOST numeric evaluator of the AD DAG
  semantics (forward + reverse VJP over W host-float lanes, no PTX emission).
- `lib/ptx/ad-dag-eval-test.f` — host gradcheck: analytic reverse vs central
  finite difference for softmax, `*.`/`+.` fan-out, and mixed row/uniform pipelines.
- `src/arch/arm64/disasm.f` — native ARM64 subset disassembler used by
  `tools/jitdump.f` and `tools/imagedisasm.f`.
- `tools/jitdump-core.f` — reusable JIT code disassembly helpers.
- `tools/jitdump.f` — disassemble a compiled word from a source snippet; see
  `docs/debugging.md` for command syntax.
- `tools/imagedisasm.f` — disassemble a raw executable byte slice by file offset.
- `tools/imgdump.f` — inspect or compare `hb` image dictionaries and snapshot
  trailers.
- `test/gate-debug.f` — gate slice for property, snapshot, and debug tooling.
- `test/proc-pty.f` — pty harness covering tty REPL behavior, `step`, and
  breakpoint/watch output.

## Mach-O And Signing

- `src/os/linux/layout.f` — Linux executable/data layout constants shared by
  syscall, image, snapshot, and inspection code.
- `src/os/linux/repl-term.f` — Linux termios constants used by baked REPL
  sources generated by `src/habu/stdin.f` and `hb-build --repl`.
- `src/os/macos/layout.f` — macOS executable/data layout constants shared by
  syscall, image, snapshot, and inspection code.
- `src/os/macos/repl-term.f` — macOS termios constants used by baked REPL
  sources generated by `src/habu/stdin.f` and `hb-build --repl`.
- `src/os/image-bytes.f` — shared executable image byte cursor, endian stores,
  patch reads/writes, and signing blob cursor helpers.
- `src/os/macos/macho.f` — Mach-O image construction.
- `src/os/macos/sign2.f` — embedded ad-hoc signature writer.
- `src/os/linux/elf.f` — dynamic Linux/aarch64 ELF executable writer.
- `src/os/linux/sign.f` — Linux no-op signing seam (ELF needs no post-link
  signature pass).
- `docs/macho.md` — Mach-O layout notes.

## Tools And Gates

- `test/gate-stats.f` — checked append-only counter log, complete exec/fork
  attribution, per-process live counters, owner-row queries, and summary helpers
  for native gate duplicate-work RCA.
- `test/gate-stats-test.f` — focused fixture for gate stats event counting,
  process choke coverage, fresh-child inheritance, and owner-local accounting.
- `test/perf-verdict.f` — pure PERF-VERDICT policy package for the frozen full-gate
  timing rule: pass/marginal/hard bands, calibration-drift stability, admissibility,
  2-of-3 retry aggregation, and a deterministic ATTEMPT-LINE over a typed attempt record.
- `test/perf-verdict-test.f` — manual-standalone acceptance fixtures for the
  PERF-VERDICT policy (band thresholds, retry aggregation, fail-closed evidence, rows).
- `test/run-verdict.f` — TR-VERDICT retry driver that runs the PERF-VERDICT frozen rule
  over real gate attempts through an injectable measure seam, renders attempt/verdict rows,
  and owns the deterministic worker machine-line channel (PA-LINE$/PA-PARSE).
- `test/run-verdict-test.f` — manual-standalone fixtures for the TR-VERDICT retry driver
  (exact attempt counts / no recursion, 2-of-3, SHA and empty-root fail-closed, machine-line round trip).
- `tools/seed.f` — checked native seed installer, SHA verifier, smoke test, and fixpoint rebuild driver.
- `tools/seed-main.f` — CLI entrypoint for checked native seed recovery.
- `tools/seed-test.f` — focused coverage for seed SHA, install, signing, and smoke helpers.
- `tools/build-fixpoint.f` — checked native stage/stdin build driver; explicit
  `snap` builds snapshot candidates for cache/debug paths.
- `tools/check-core.f` — reusable Habu-native checked engine runner core.
- `tools/check.f` — thin CLI entrypoint for the checked engine runner.
- `tools/check-main.f` — no-include checked engine entry for checker CLI reuse.
- `tools/check-test-lib.f` — reusable checked fixture library for check runner semantics.
- `tools/check-test.f` — checked fixture coverage for the native check runner.
- `tools/sha256-file-test.f` — checked fixture coverage for streaming SHA-256 helpers.
- `lib/content-key.f` — checked manifest-hash builder for content-addressed gate caches.
- `lib/content-key-test.f` — checked fixture coverage for content-key stability and invalidation.
- `lib/engine-id.f` — checked engine self-identity: kernel-resolved own executable path + lazy SHA-256 content key over bin/hb.
- `lib/engine-id-test.f` — checked fixture coverage for the engine self-path and content-key words.
- `lib/object.f` — checked OBJ package object-record codec for future linkable builds.
- `lib/object-test.f` — focused coverage for object-record serialization, loading, and keys.
- `lib/object-cache.f` — checked OBJSTORE content-addressed file store for
  validated object records.
- `lib/object-cache-test.f` — focused coverage for OBJSTORE path, store, load,
  malformed-file, and missing-key behavior.
- `lib/object-index.f` — checked OBJIDX source-to-object key index for build
  cache lookup before object recompilation.
- `lib/object-index-test.f` — focused coverage for OBJIDX source-key stability,
  store/load, misses, and malformed-index behavior.
- `lib/object-resolve.f` — checked OBJRES source+ABI resolver over the object
  index and content-addressed object store.
- `lib/object-resolve-test.f` — focused coverage for OBJRES store/load,
  misses, wrong-index, and stale-object failures.
- `lib/object-link.f` — checked OBJLINK export/import symbol validation over
  loaded object records.
- `lib/object-link-test.f` — focused coverage for resolved imports, duplicate
  exports, unresolved imports, and symbol-table overflow.
- `tools/object-image.f` — checked OBJIMG build-internal wrapper that turns
  linked object text into a target native executable image.
- `tools/object-image-test.f` — focused coverage for writing and running a
  tiny executable from object text.
- `tools/hb-cli-contracts-test.f` — checked coverage for `hb` startup and stdin-data contracts.
- `tools/standalone-load-test.f` — proves lint/tool core entries load in isolation via hb --load child spawns (each entry requires its own deps).
- `tools/hb-baseline-contracts-test.f` — checked public `bin/hb` baseline contract fixture.
- `tools/build-cache-probe.f` — isolated child fixture for canonical cache-root
  environment resolution.
- `tools/hb-build-lib.f` — checked native AOT/REPL build CLI library.
- `tools/hb-build-report.f` — checked lifecycle-safe typed report state plus
  dynamically sized success and cache-root failure wire renderers.
- `tools/hb-build-direct-lints.f` — optional in-process lint hook adapter for
  hb-build gate callers that already loaded lint cores.
- `tools/hb-build.f` — Habu entrypoint for native AOT/REPL builds.
- `tools/hb-build-test.f` — checked fixture coverage for native REPL builds and
  hb-build boundary rejections.
- `tools/cli-run.f` — checked helpers for explicitly installed CLI fixture
  subprocesses.
- `tools/bootstrap-codegen-test.f` — native source regression for bootstrap
  codegen fail-closed contracts, including byte-span parity for the recovery
  compile-preflight diagnostic.
- `tools/imgdump.f` — native image dictionary dump and compare tool.
- `tools/imgdump-test.f` — checked fixture coverage for image dump compare mode.
- `tools/imagedisasm.f` — native raw image slice disassembler.
- `tools/imagedisasm-test.f` — checked fixture coverage for raw image disassembly.
- `tools/include-events-test.f` — checked fixtures for the source-composition event log and loader instrumentation.
- `tools/source-discovery.f` — whole-file source-composition discovery pass that lexes the entire token stream (colon bodies included), replays every literal loader form against a fresh require registry, and emits the ordered event artifact; dynamic paths, loader shadow/undefine/retirement, and unsupported openers reject fail-closed unless the entry is a declared dynamic-tail boundary.
- `tools/source-discovery-test.f` — checked fixtures for the whole-file discovery pass (ordering, multiplicity, dedup, fresh registry, colon-body capture, byte-exact spans, shared emitter, fail-closed rejection, dynamic-tail manifest boundary).
- `tools/source-arena-policy.f` — checked shared headroom and power-of-two policy for native, stage2, and maker source arenas.
- `tools/dynamic-tail-manifest.f` — declared dynamic-tail boundary table (path + reason) consumed by the discovery pass; a listed file's dynamic/retired loader forms are tolerated instead of rejected.
- `tools/event-closure-lib.f` — ordered transitive source-composition closure list built by replaying the discovery pass breadth-first over the event log.
- `tools/event-closure-test.f` — checked fixtures for the closure list (order, dedup, transitive descent, provided/missing exclusion, colon-wrapped deps) and closure key sensitivity.
- `tools/ptx/saxpy.f` — CLI entrypoint that emits the M3 SAXPY PTX kernel.
- `tools/ptx/saxpy-test.f` — checked fixture for the PTX SAXPY encoder output.
- `tools/ptx/ptxas-smoke.f` — Orin-only checked smoke that emits SAXPY PTX,
  runs `ptxas`, and removes generated `.ptx`/`.cubin` artifacts.
- `tools/ptx/kernel-export.f` / `tools/ptx/kernel-export-lib.f` /
  `tools/ptx/kernel-export-test.f` — `hb kernel-export` CLI and library
  (`package KEXPORT`): captures a registered producer's PTX in-process and
  writes the versioned `<NAME>.ptx` + `<NAME>.manifest.json` artifact pair an
  external build embeds (see `examples/kernel-consumer/`). Host-only and
  deterministic; named `E-KEXPORT-*` errors. Tests spawn the CLI twice and
  byte-compare both artifact pairs, pin manifest/PTX fragments, and cover the
  unknown-kernel and bad-out-dir negatives in-process.
- `examples/kernel-consumer/build.zig` / `examples/kernel-consumer/main.zig` —
  descriptive external Zig consumer of the exported artifact pair: embeds
  PTX + manifest, verifies both hashes, and maps every manifest field to its
  CUDA Driver API argument (`cuModuleLoadData`/`cuModuleGetFunction`/
  `kernelParams` from `param_slots` order/`cuLaunchKernel` geometry). Not
  built by this repo.
- `tools/ptx/saxpy-cg.f` — checked SAXPY kernel body run through the PTX codegen
  vocabulary.
- `tools/ptx/saxpy-v4-tail-device-test.f` — Orin device proof that checked v4
  SAXPY residual scalar lanes are correct for `n=4,5,7,1000003`.
- `tools/ptx/device-gold.f` — committed device-correctness goldens for the four
  flagship kernels (GAP #4, habu-committed-device-correctness): spawns bin/hb to
  emit each COMMITTED entry file (`tools/ptx/matmul-cg.f` MM,
  `tools/ptx/attention-cg.f` ATTN, `tools/ptx/fused-relu-cg.f` relu(a*x+y),
  `tools/ptx/saxpy-cg.f` bandwidth), ptxas-assembles, launches on the Orin, and
  compares a committed CPU golden on exact-binary-fraction inputs (SGEMM A*B with the
  lower-golden atol+rtol matmul tolerance; attention colmean(V); fused/bandwidth with
  the eval-device scalar TOL). Off-device a recorded SKIP that still check-loads;
  SPAWN-ONLY ptx-toolchain member; device-runs on zed by loading this file. A wrong
  kernel FAILS its golden.
- `tools/ptx/device-gold-test.f` — HOST proof of device-gold's emit halves (no
  device, no ptxas): spawns each committed entry file and asserts the emitted PTX is
  the right kernel body (MM has FMA + cp.async; ATTN has ex2.approx + bar.sync; fused
  has the relu max.f32 clamp; bandwidth SAXPY has scale+bias but no clamp), plus a
  fail-closed missing-producer throw. Inprocess ptx-toolchain member; mirrors
  `tools/ptx/fusion-emit-test.f`.
- `tools/ptx/smem-cg.f` — checked shared-memory tile body run through the PTX
  codegen, proving `COOP-CTX`/`STAGE`/`SLOAD`/`SSTORE` emit barriers and shared
  loads/stores.
- `tools/ptx/ops-cg.f` — checked scalar/v4 elementwise op kernel bodies run
  through the PTX codegen vocabulary.
- `tools/ptx/once-cg.f` — checked once-space load/store kernel body proving
  read-once witness stores lower to ordinary `st.global` rather than scatter-add.
- `tools/ptx/acc-cg.f` — checked AXPY-ACC kernel (register-accumulator ops) run through
  the PTX codegen; lowers ACC-ZERO/ACC-FMA/ACC-TILE to mov/fma/identity.
- `tools/ptx/acc-device-test.f` — committed device-correctness regression: the checked
  accumulator kernel emits, ptxas-assembles, and computes x*y=6.0 on the Orin.
- `tools/ptx/gradcheck.f` — device-run central-difference gradcheck (the AD hard gate):
  gates SAXPY (linear, dx=a) and RELU (nonlinear, dx=step) on the Orin vs the analytic VJP;
  wrong VJPs rejected. Retains the primary context once, releases once (or bin/hb hangs at exit).
- `tools/ptx/relu-cg.f` — checked RELU kernel (y=max(x,0)) run through the codegen; the
  nonlinear op gradcheck.f gates.
- `tools/ptx/exp-cg.f` — checked EXP kernel (y=exp(x)) run through the codegen; the
  transcendental op gradcheck.f gates (d exp/dx = exp(x), non-constant gradient).
- `tools/ptx/expbwd-cg.f` — checked EXP backward kernel (dx=dz*savedy, the SAVED-Y→real-load
  resolution); gradcheck.f runs it on device and checks its output = the numeric gradient.
- `tools/zed-run-lib.f` — checked remote device-run harness (`package ZED`): argv-spawned
  ssh/scp/rsync via lib/process-command.f, private scratch-dir lifecycle, remote command
  capture (stdout/stderr/rc), failures mapped to named E-ZED-* throw codes, and the HABU_ZED
  availability/skip policy. No shell scripts, no interpolated remote test input.
- `tools/zed-run.f` — CLI probe over the harness: joins script argv into one remote command,
  runs it on the ZED host, echoes captured output, and exits fail-closed on failure.
- `tools/zed-run-test.f` — checked unit tests (availability policy, command construction,
  outcome classification) plus HABU_ZED-gated device smokes (`true` rc0, `false` fail-closed).
- `tools/ptx/redadd-cg.f` — raw-PTX emit driver for a `red.global.add.f32` kernel (each thread
  atomically adds 1.0 to out[0]); the scatter-add primitive reverse-mode fan-in adjoints need.
- `tools/ptx/redadd-device-test.f` — Orin device proof that `red.global.add.f32` assembles for
  sm_87 and accumulates correctly (256 atomic adds = 256.0); closes habu-ptx-ad-verify.
- `tools/ptx/scatter-add-grad-cg.f` / `tools/ptx/scatter-add-gradcheck.f` —
  checked fan-in context kernels plus Orin device gradcheck: finite difference
  of `sum_i x[0]` matches the analytic scatter-add backward `dx[0] = n`.
- `tools/ptx/indexed-scatter-cg.f` / `tools/ptx/indexed-scatter-gradcheck.f` —
  checked generic indexed gather/scatter kernels plus Orin device gradcheck:
  duplicate `idx[i]` accumulation is proven by finite difference and analytic
  `INDEX-SCATTER-ADD`.
- `tools/ptx/sum-cg.f` / `tools/ptx/sum-launch.f` — checked direct row-sum
  codegen plus Orin device proof for BLOCK-SUM's reducer-local inactive-lane zero.
- `tools/ptx/sum1024-cg.f` — checked direct row-sum text fixture proving `%BLOCK
  1024` changes shared-memory size and reduction fold bounds.
- `tools/ptx/zed-device-suite.f` — Orin device proof of the collective fix:
  runs the SELF-EMITTING launchers (sum-launch / softmax-launch /
  softmax-gradcheck) on the Orin over the ssh harness and asserts a zero exit;
  each launcher emits+ptxas-assembles+launches+compares its own kernel fail-closed,
  so the harness no longer ships local /tmp/zed-*.ptx or remote-builds shared
  /tmp/*.cubin. HABU_ZED-gated.
- `tools/ptx/launch-neg-test.f` — fail-closed regressions for the launch/emit
  contracts: malformed WHERE (E-PTX-SYNTAX), block mismatch and k > block
  (E-PTX-BLOCK); the same header.f/launch.f contracts the device goldens launch under.
- `tools/ptx/softmax-cg.f` / `tools/ptx/softmax-bwd-cg.f` — checked
  SOFTMAX-ROWS forward/backward emit drivers.
- `tools/ptx/softmax-fb-cg.f` — combined driver emitting ONE PTX module with both
  the forward SOFTMAX_ROWS and the AD-derived SOFTMAX_BWD entries under a single
  header, so softmax-gradcheck loads a single cubin and pulls both handles from it.
- `tools/ptx/ad-entry-lib.f` — per-VJP-entry kernel emitters for the device
  gradcheck gate: DAG op-lists isolating each ad-dag entry (EXP, x-max, x/sum,
  full softmax) plus the vjp.f table fixtures - two-input elementwise
  (+./-./*.//. via AD2_FWD/AD2_BWD), scalar-factor SCALE/FMA. (ADS_*/ADF_*),
  the OVER fan-out composite and DROP composite - and the deliberate wrong
  variants (fan-out dropped, OVER-as-permutation, DROP cotangent leak); text
  shape asserted in saxpy-test.f.
- `tools/ptx/ad-gradcheck-launch.f` — Orin-side per-VJP gradcheck launcher:
  central differences over each emitted forward (both inputs and the scalar
  factor) vs the analytic backward, per-element rtol+atol, tie and saturated
  fixtures, poisoned readbacks, every CUDA rc checked; the wrong variants
  (fan-out dropped, OVER-as-permutation, DROP leak, cross-pair) must mismatch.
- `tools/ptx/zed-gradcheck-suite.f` — Mac orchestrator for the per-VJP device
  gradcheck gate: emits all entry kernels, ships/assembles via the ssh harness,
  proves malformed-PTX and missing-cubin failure classes red, then runs the
  launcher on the Orin. HABU_ZED-gated.
- `lib/ptx/ad-gen.f` / `lib/ptx/ad-gen-test.f` — lowering of a GENERATED
  straight-line body (the reverse pass output, AD-BACKWARD$) to PTX kernel
  compute: token-driven EMIT dispatch over an emit-time register stack, with
  SAVED-* resolution by row-local recompute of the forward slice (bindings for
  X/Y/Z/MX/S/A; ZERO. lowers to a fresh zero tile); fail-closed v0 contract
  (one load, one final store/scatter, at most one saves-op per forward,
  unbound SAVED-*, unknown tokens and unbalanced bodies reject) plus the
  composed pass tests (generated XSUBSUM backward text, NEG NEG collapse,
  control-flow rejects, saves-op scan).
- `lib/ptx/ad-ir.f` / `tools/ptx/softmax-bwd-opt-cg.f` — AD-op-list to PTX-IR
  bridge plus closed-form SOFTMAX backward emitter for the saved-output path.
- `tools/ptx/softmax-rows-bwd-cg.f` — the ad-reverse capstone: the CHECKED
  closed-form SOFTMAX-ROWS-BWD (dx = y*(dy - Sum(dy*y)), the reverse-pass +
  simplifier derivation asserted in ir-test.f) certifies with token-shared
  extents and emits its own SOFTMAX_BWD_ROWS kernel for the device gradcheck.
- `lib/ptx/autograd-neg-test.f` — the gradient extent contract is static:
  shared-extent closed-form backward and the MK-SPAN= minted gradient pair
  certify; a dx typed with a different extent and a separately minted gradient
  span are checker REJECTS (len(dx)=len(y) proven by token, never re-asserted).
- `lib/ptx/header.f` / `lib/ptx/header-test.f` — checked PTX kernel-header
  vocabulary and its coverage.
- `lib/ptx/kernel-abi.f` / `lib/ptx/kernel-abi-test.f` — structured kernel-ABI
  record (`package KABI`), the single source of truth for a kernel's entry
  name, block, grid-derivation token, ordered logical params (span/matrix/
  uniform), and the derived flat `.param` layout (offset/size/type/role/source,
  equal-extent-token dedup); `CG-ENTRY`/`CG-PARAMS`/`CG-RESET` render and seed
  from it and `tools/ptx/cuda-launch.f` packs launch offsets from it. Tests pin
  the SAXPY layout to the historical hand literals plus matrix/index-span
  derivations and named-error negatives.
- `lib/ptx/kernel-manifest.f` / `lib/ptx/kernel-manifest-test.f` —
  habu-kernel-manifest v1 JSON renderer (`package KMAN`): the active KABI
  record + module-target accessors + hashed (never parsed) PTX text render the
  versioned export manifest via `lib/json-write.f` in fixed field order
  (contract: docs/ptx-sketch.md "Kernel ABI contract"). Tests pin schema
  fragments, per-kind lowering, both hashes (the content-hash contract is
  recomputed mechanically), byte-determinism, and the unnamed-record negative.
- `lib/ptx/rep.f` / `lib/ptx/rep-test.f` / `lib/ptx/rep-neg-test.f` —
  phantom-preserving register-emitter combinators (`PTXREP:REP1`/`REP2`/
  `REPMIX2`) that carry a kernel token's `n` register through a checked emitter
  while preserving its phantom type, so type-preserving tile/collective ops
  certify as checked instead of TRUSTED:. Also the leg-2b MINTING combinators
  (`PTXREP:MINT-LOAD`/`MINT-ROW-SPAN`/`MINT-ROW-LOAD`) that repackage operand
  registers into a NEW projected phantom. Positive coverage plus the
  forge/kind/arity negative regressions (dot habu-ptx-phantom-preserving).
- `lib/ptx/mint-test.f` / `lib/ptx/mint-neg-test.f` — positive + negative
  coverage for the leg-2b checked-mint capability (`src/core/checker.f`
  NP-MINT-CHECK) and the `PTXREP:MINT-*` combinators: legit projected mints
  certify, free-typed / wrong-family forges reject fail-closed (dot
  habu-ptx-phantom-preserving).
- `lib/ptx/tile.f` / `lib/ptx/tile-test.f` — PTX tile-DSL v0 operation
  vocabulary (M4) and the checked SAXPY proof.
- `lib/ptx/tile-loop.f` / `lib/ptx/tile-loop-test.f` /
  `lib/ptx/tile-loop-neg-test.f` — checked counted-loop combinator for tile
  kernels plus positive and negative regressions.
- `lib/ptx/tile-smem.f` / `lib/ptx/tile-smem-test.f` /
  `lib/ptx/tile-smem-neg-test.f` — checked shared-memory tile vocabulary plus
  positive and negative regressions.
- `lib/ptx/tile-acc.f` / `lib/ptx/tile-acc-test.f` /
  `lib/ptx/tile-acc-neg-test.f` — checked register-accumulator vocabulary plus
  positive and negative regressions.
- `lib/ptx/tile-v4.f` / `lib/ptx/tile-v4-test.f` — vectorized (v4) tile-DSL
  operations and the checked v4 SAXPY proof.
- `lib/ptx/tile-v4a.f` / `lib/ptx/tile-v4a-test.f` /
  `lib/ptx/tile-v4a-neg-test.f` — M10 alignment-proven typed vec4 vocabulary
  (`vspan`/`vtile`, `V4-ALIGN`/`LOAD.V4`/`STORE.V4`) whose 16B-alignment and
  lane-arity legality the checker proves, plus positive/emit and negative
  regressions.
- `lib/ptx/tile-pipe.f` / `lib/ptx/tile-pipe-test.f` /
  `lib/ptx/tile-pipe-neg-test.f` — typed pipelined register-blocked GEMM tile
  vocabulary (`mmstage`/`mmaslice`/`mmbslice`/`mmafrag`/`mmbfrag`/`mmracc`,
  `PIPE-LOOP`/`STAGE-SLICES`/`A-FRAG`/`B-FRAG.V4`/`RB-FMA`/`K-UNROLL`) over the
  cg-matmul-emit.f MM-* emitters, byte-identical to EMIT-MATMUL by test, with
  parity/alignment/layout/naive-path negative regressions. The trusted core is
  the phantom MINTS only; `RB-FMA` and `PIPE-STORE` (operand-consuming, no mint)
  are checked and the compute-slot adapter folded into `PIPE-LOOP`.
- `lib/ptx/cpp-slot.f` / `lib/ptx/cpp-slot-test.f` /
  `lib/ptx/cpp-slot-neg-test.f` — the cp.async pipeline-slot TYPESTATE (package
  CPPSLOT): a staged-buffer slot threads `cpp-pending<p>` -> `cpp-committed<p>`
  -> `cpp-ready<p>` (p = buffer parity), so the dynamic protocol ordering is a
  CHECKED discipline (`COMMIT`/`WAIT`/`READ`/`READ-STAGE`). The four dynamic
  negatives — read-before-wait, missing-commit, double-wait, parity mismatch —
  plus the divergent-barrier negative reject fail-closed; `WAIT`'s bar.sync fence
  composes with the M5 barrier model. Owns habu-checker-cp-async-6ba788a5.
- `lib/ptx/collective.f` / `lib/ptx/collective-test.f` — tile-DSL row and
  collective vocabulary (M6) plus the checked stable-softmax proof.
- `lib/ptx/uniform-barrier-test.f` — M5 uniformity + block-uniform barrier model:
  `uniform<T>` vs lane-varying `tile` rejects (tile used where a uniform is
  required), and a block collective (`BLOCK-MAX`/`BLOCK-SUM`, tile-in/uniform-out
  = a `bar.sync` reduction) reached under open control (if/begin/do) rejects as a
  divergent barrier, while the straight-line softmax/broadcast kernels certify.
- `lib/ptx/gemm-checked-test.f` / `lib/ptx/gemm-checked-neg-test.f` — the
  PRODUCTION tiled GEMM certification proof (cg-matmul.f MM-CHECKED is the
  shipped typed kernel) plus negative regressions: inline non-neutral K-loop,
  missing MM-STORE, swapped A/B operands.
- `lib/ptx/attention-checked-test.f` / `lib/ptx/attention-checked-neg-test.f` —
  checked fused-attention matrix/phase proof, byte-stable emitter regression,
  and phase/shape negative regressions.
- `lib/ptx/attention-roles-test.f` — operand-role regression: two candidates
  differing only in Q/K/V/O order must emit DIFFERENT PTX (the attnctx threads
  each operand's pointer register), while correct authoring stays byte-stable.
- `lib/ptx/cg.f` / `lib/ptx/cg-vec.f` / `lib/ptx/cg-collective.f` /
  `lib/ptx/cg-collective-test.f` / `lib/ptx/cg-matmul-emit.f` /
  `lib/ptx/cg-attention.f` — PTX codegen emit-mode lowering for tile ops:
  scalar, vectorized v4, row/collective, the register-blocked SGEMM emitters
  (the byte-sensitive MM-* surface shared verbatim by lower-mm.f and cg-mma.f),
  and the fused attention kernel. The fused cp.async K-loop is decomposed into
  shared `CPP-*` protocol STEP emitters (issue via MM-CP-STAGE, `CPP-COMMIT` /
  `CPP-WAIT` / `CPP-SYNC` barrier / `CPP-CUR-WINDOW` read-window / `CPP-FLIP`
  parity) that MM-PIPE-KLOOP-WITH and cg-mma.f's K-loops compose byte-identically,
  shaped for the cp.async typestate obligations (habu-checker-cp-async-6ba788a5).
  The cg-collective test pins the block-reduction
  emit shape: the two-level warp shfl.sync.down reduction with the inactive-lane
  identity seed threaded through both shuffle levels.
- `lib/ptx/cpp-pipe-step-test.f` — exact-byte regression for those decomposed
  `CPP-*` cp.async pipeline step emitters: each step's PTX fragment is pinned via
  PTX-CAPTURE, so a drift in any single protocol step fails closed (the
  tile-pipe-test.f byte-capture pattern at step granularity).
- `lib/ptx/cg-matmul.f` — the PRODUCTION checked tiled GEMM: EMIT-MATMUL ships
  the certified KERNEL: MM-CHECKED composed from the tile-pipe vocabulary
  (MM-BEGIN / MM-K-LOOP / MM-STORE, the eval lane's GEMM authoring words); the
  only trusted word is the launch-ABI mint MM-ABI.
- `lib/ptx/cg-matmul-naive.f` — the naive one-element-per-thread SGEMM baseline
  kernel (MMN) that the GEMM benchmark measures the register-blocked tile against.
- `lib/ptx/cg-mma.f` — TF32 tensor-core (mma.sync.aligned.m16n8k8) tiled GEMM
  (MMM): same 64x64 block and cp.async double-buffered staging as the register-
  blocked SGEMM, the 4x4 fma micro-tile swapped for warp-level MMA tiles (the
  compute-roof beat-Triton lever). The single-buffer (stages=1) K-loop threads
  the CPPSLOT typestate: `MMA-STAGE-ISSUE` (the trusted cp.async issue mint)
  -> `CPPSLOT:COMMIT`/`WAIT`/`READ`, so the per-iteration protocol ordering is
  checker-enforced, byte-identical to the fused steps.
- `lib/ptx/cg-mma-slot-neg-test.f` — production-shaped falsification
  regressions for that checked single-buffer protocol: the in-order shape
  certifies; wait-before-commit, dropped-wait, and read-after-issue reject on
  the real `MMA-STAGE-ISSUE` mint (the read-after-issue leg pins the mint's
  cpp-pending state, the anti-tautology guard).
- `lib/ptx/cg-activation.f` — PTX codegen for the gelu/silu elementwise
  activations, mirroring the maki host references op-for-op for f32 golden parity.
- `lib/ptx/ad.f` / `lib/ptx/ad-test.f` — reverse-mode autograd transform v0
  and its runnable tests.
- `lib/ptx/ad-saved.f` / `lib/ptx/ad-saved-test.f` — typed saved-value
  vocabulary for auto-derived backward kernels and its checked coverage.
- `lib/ptx/autograd-test.f` — checked verified-gradient kernel regression.
- `tools/ptx/emit.f` — checked PTX text encoder behind the emit drivers.
- `tools/ptx/saxpy-v4-cg.f` / `tools/ptx/relu-v4-cg.f` /
  `tools/ptx/fused-relu-cg.f` / `tools/ptx/maxselect-cg.f` /
  `tools/ptx/matmul-cg.f` / `tools/ptx/attention-cg.f` — checked kernel emit
  drivers for v4 SAXPY, v4 RELU, fused RELU, max-select, tiled SGEMM, and
  fused attention.
- `tools/ptx/profile-test.f` / `tools/ptx/bench-test.f` — focused coverage for
  PTX benchmark/profile math and configuration.
- `lib/test.f` — public checked test framework interface: assertions plus
  the `TEST:*` suite/group/test package facade.
- `lib/test/assert.f` — checked assertion primitives used by test fixtures.
- `lib/test/budget.f` — separate load-aware test budgets: `T-BUDGET-MS` uses structural `HB_LOAD_PCT` for child timeouts; `TEST-BUDGET:PERF-MS` uses measured `HB_CAL_PCT` for performance ratchets; both self-calibrate standalone and clamp at 3x.
- `lib/test/budget-test.f` — focused coverage for budget factor math and standalone self-calibration.
- `lib/test/assert-test.f` — focused coverage for checked assertion primitives.
- `lib/test/record.f` — machine-readable `TFAIL` TSV failure records shared by
  the assert, snapshot, and runner test layers.
- `lib/test/record-test.f` — focused coverage for failure-record format and
  capacity guards.
- `lib/test/snap.f` — shared `T{ ... -> ... }T` stack-snapshot assertions
  aggregated into the assert counters.
- `lib/test/snap-test.f` — focused coverage for stack-snapshot pass, mismatch,
  label, and capacity-guard behavior.
- `lib/test/suite.f` — private implementation body included by `lib/test.f`
  inside package `TEST`.
- `lib/test/suite-test.f` — focused package-scoped coverage for `TEST:*`
  setup/teardown hooks, groups, tests, stdin tests, filters, and argument feeds.
- `lib/test/subject.f` — reusable fork-evaluate capture boundary for testing
  source against the running engine without booting a nested engine process.
- `lib/test/subject-test.f` — focused capture, dictionary-isolation,
  inherited-handler, diagnostic, and timeout coverage for `SUBJECT:RUN`.
- `lib/test/src-shape.f` — shared `package SHAPE` source-shape assertions: LOAD
  auto-sizes an OS-backed buffer to a whole source file (fail-closed CHECK-FIT
  diagnostic above the ceiling), then HAS?/MUST-HAVE/MUST-LACK/COUNT=/COUNT
  assert its shape. Removes the divergent per-test CAP/BUF/READ-ALL machinery.
- `lib/test/src-shape-test.f` — focused coverage for the source-shape helper:
  fail-closed CHECK-FIT, auto-sized LOAD roundtrip, and HAS?/COUNT assertions.
- `tools/ptx/cuda-launch.f`, `tools/ptx/softmax-launch.f`, and
  `tools/ptx/softmax-gradcheck.f` — Orin CUDA Driver proofs for launch,
  softmax, and finite-difference gradient checking; each SELF-EMITS its checked
  producer to a private per-run PTXTC cubin fail-closed (E-PTX-EMIT), never a
  shared /tmp cubin. cuda-launch also proves f32 scalar marshalling host-side.
- `tools/ptx/profile.f`, `tools/ptx/bench.f`, `tools/ptx/bandwidth-lib.f`,
  `tools/ptx/bandwidth.f`, `tools/ptx/bandwidth-v4.f`, and
  `tools/ptx/fusion-compare.f` — reusable Orin kernel profile metrics, generic
  CUDA Driver launch plus CUDA-event device timing, scalar/v4 SAXPY bandwidth,
  and fused-vs-unfused kernel comparison for the Habu-PTX column. bandwidth /
  bandwidth-v4 SELF-EMIT their SAXPY cubin (fusion-emit PTXFE:BUILD-KERNEL,
  fail-closed) and SKIP off-device; no shared /tmp/saxpy.cubin default.
- `tools/ptx/fusion-emit.f` / `tools/ptx/fusion-emit-test.f` — self-emit the
  checked v4 SAXPY/RELU/fused-RELU cubins to private per-run toolchain roots for
  fusion-compare (fail-closed `E-PTX-EMIT` on a missing producer or nonzero
  emit/ptxas rc, never a stale shared `/tmp` cubin), and the host proof of that
  emit half (no device, no ptxas — safe in-process).
- `tools/ptx/bandwidth-lib-test.f` — host-side coverage for the bandwidth
  runner configuration math (device leg is a recorded SKIP off-device).
- `tools/ptx/perf-rows.tsv` — durable kernel profile-row registry: one row per
  kernel-optimization rung/shape/metric (GBS, GFLOPS, PCT-ROOF, WAIVER) with
  launch config, device, and date; owned by `tools/ptx/perf-registry.f`.
- `tools/ptx/perf-registry.f` — checked parser/validator for the profile-row
  registry (package PERF): row model, TSV parse, fail-closed row validation.
- `tools/ptx/perf-registry-test.f` — registry fixtures plus validation of the
  committed `tools/ptx/perf-rows.tsv`.
- `tools/ptx/perf-compare.f` — perf-regression compare over registry rows:
  latest same-key pair per kernel+config+device+metric vs `PERF:TOL-MILLI`.
- `tools/ptx/perf-compare-test.f` — improve/regress/tolerance-edge/missing-row
  compare fixtures.
- `tools/ptx/perf-regress.f` — CLI perf-regression gate over the committed
  registry; exits nonzero on any regression. Resolves its registry path from
  ambient `SCRIPT-ARGV`, so it is spawn-only (clean argv in a fresh image).
- `tools/ptx/perf-regress-test.f` — argv-free checked fixture carrying the
  substantive in-process regression scan (committed-registry `PERF:LOAD` +
  `PERF:SCAN`) plus the hermetic-`PERF:RESET` regression: a failed `PERF:LOAD`
  after prior fixtures must not leak a stale line via `PERF:LAST-LINE$`.
- `tools/ptx/gemm-bench.f` — CUDA-event GEMM benchmark: times the naive (MMN)
  and register-blocked (MM) SGEMM kernels on square shapes for the
  GEMM-vs-Triton baseline recorded in `docs/eval-triton.md`.
- `tools/ptx/attention-bench.f` / `tools/ptx/attention-bench-test.f` — CUDA-event
  benchmark of the fused ATTENTION kernel (producer `tools/ptx/attention-cg.f`):
  self-emits + ptxas-assembles ATTN to a private per-run toolchain root via
  `tools/ptx/fusion-emit.f` (fail-closed `E-PTX-EMIT`), stages a deterministic
  non-uniform f32 pattern into Q/K/V, launches grid=N block=N over the N=64,128
  D=64 ladder, and reports GFLOP/s (`4*N*N*D + 5*N*N` flops/launch); device-gated
  on `CUDA:OPEN?` (recorded SKIP off-device). The test proves the emit half
  host-side (ATTN entry + phase/softmax/writeback markers, no ERROR leak).
- `tools/ptx/mma-probe.f` — single-warp TF32 `mma.sync.aligned.m16n8k8` fragment-
  layout isolation proof: verifies ONE MMA element-exact vs a host matmul before
  any tiling (the course's #1 "correct in NumPy, garbage on device" guard).
- `tools/ptx/mma-gemm-check.f` — device-correctness of the full K-looping TF32
  mma.sync GEMM kernel (MMM) element-exact vs a host matmul at 64^3 and 128^3
  (staging + accumulation + the warp/D-fragment store mapping), plus the fp16 and
  bf16 `m16n8k16` tiles (`MGC-CFG-F16`/`MGC-CFG-BF16` and their transposed-Bs `-T`
  variants, both warp grids + epilogue + both B feeds) and the emit fail-closed
  guards (SMEM / BLDM / WARPS / EPI / DTYPE / BTF16 / bf16 negatives).
- `tools/ptx/mma-emit-diff.f` — byte-identity harness for the mma.sync GEMM
  emitter: emits `EMIT-MATMUL-MMA` for 35 TF32 configs (default / SWZ / dyn / wide
  / wide-B / 4-warp / deep-stage / epilogue) with a per-config header so the stream
  can be diffed before vs after a change; with `MMA-DTYPE=0` the fp16/bf16 tile
  additions leave it empty (6 fp16 + 6 bf16 rows appended after the TF32 stream).
  Device-independent (pure emit).
- `tools/ptx/mma-profile.f` — profile-first harness for `lib/ptx/cg-mma.f`: emits
  ONE tile config (BK / pad / stages / dyn-smem / fragment mode, config-driven via
  `-- BK PAD STAGES DYN MODE SHAPE`, defaulting to the swizzled ldmatrix best) and
  launches MMM EXACTLY once (no timing loop, no warmup) so an external profiler
  (`ncu -k MMM --launch-count 1`) captures a single clean tensor-core launch;
  device-gated on `CUDA:OPEN?` (recorded SKIP off-device). Correctness is owned by
  `tools/ptx/mma-gemm-check.f`, timing by `tools/ptx/gemm-bench.f`; this only shapes
  the launch.
- `tools/ptx/mma-ablate.f` — DCE-safe timing decomposition of the wider-M TF32
  mma.sync GEMM (`lib/ptx/cg-mma.f` `MMA-ABLATE` knob): emits the 128x64 wide config
  at each ablation mode (full / quarter-B / half-B / single-mma), each keeping every
  mma + store live so ptxas cannot delete the ablated work, and times them
  same-session to attribute per-phase cost (B-feed, mma-issue) and the quarter-B
  ceiling — the iGPU has no counter profiling, so ablated variants are the method.
  Device-gated on `CUDA:OPEN?` (recorded SKIP off-device); results in ablated modes
  are numerically WRONG on purpose (correctness owned by `tools/ptx/mma-gemm-check.f`).
- `maki/README.md` / `maki/STATUS.md` — Maki framework overview and current
  verification status outside the Habu trust root.
- `maki/mha.f` — multi-head causal self-attention sublayer forward (single
  sequence T, channels C, H heads, head dim hd), authored as `SPEC:` contraction
  lines (QKV projections, per-head scores, A.V, and a head-merge output
  projection over the composite head/head-dim index) composed by plain checked
  colon words with named row ops: the inverse-sqrt-head-dim scale (`maki/attention.f`
  ATTN-SCALE!) and the causal-masked row softmax (`maki/causal.f`
  CAUSAL-SOFTMAX-ROWS), plus a named bias add and residual add. Toy fixed extents;
  real shapes/batch arrive via PROMOTE + the extent-role product capability.
- `maki/mha-test.f` — exact numeric parity of `MHA-FWD` against a reference
  composed from the existing goldens (MATMUL/MM-NT/ATTN-SCALE!/
  CAUSAL-SOFTMAX-ROWS), per-head slicing proof (head-major output blocks match the
  per-head reference and stay isolated when one head's weights move), causal-mask
  proof (a perturbed future position cannot change an earlier query's output),
  named-throw negatives for malformed specs, and checker rejects of swapped-operand
  candidates on both the transposed Q.K^T and the rank-3 head-merge contraction.
- `maki/cad-kinds.f` — package-scoped nominal identities for Model CAD handles,
  indexes, shape/layout domains, effects, regions, the canonical artifact
  envelope provenance roles (artifact-kind, producer/config/numeric-policy/
  capability ids, persistent audit-event-id), and the machine-facing action-id
  (owner package ACTION, maki/db/action.f).
- `maki/cad-kinds-test.f` — qualified identity, cross-role rejection, typed
  memory, and repair-diagnostic regressions for the nominal CAD kinds, plus the
  frozen artifact-envelope invariants: class-vs-identity, audit-event vs runtime
  ADAG:event-id, artifact-kind separation, 256-bit digest vs artifact-id, and the
  action-id vs producer/schema/artifact-id nominal separation.
- `maki/target/target.f` — immutable target descriptors, semantic interning,
  canonical facts/digests, and the validated `CAD-KIND:target-id` owner API, plus the
  `ID>WIRE`/`WIRE>ID` (raw) and cross-process `KEY>WIRE`/`WIRE>KEY` codec (32-byte
  SHA-256 over the canonical `DESC-FACTS$`, § 23.9 origin-class table).
- `maki/target/target-test.f` — target identity, descriptor validation,
  round-trip, capability non-aliasing, role rejection, and privacy regressions.
- `maki/artifact.f` — the built-artifact identity registry: `ARTIFACT:REGISTER`
  interns a section-7.4 store key to a validated `CAD-KIND:artifact-id` (content
  addressed: equal keys share one id), plus `KEY$`/`EQUAL?`/`VALIDATE-ID`/`COUNT`. The
  public producer the R7 evidence/policy layer was missing (retiring the test-only
  `T>AID` fabrication); raw conversions stay private (the target.f pattern). Also owns
  the cross-process `KEY>WIRE`/`WIRE>KEY` codec (§ 23.9 origin-class table): the 32-byte
  SHA-256 content key over the interned store key, resolved by content in any process
  (the digest-covered id + dependency wire form the envelope migrates to). Owns
  -5244..-5246 + -5248.
- `maki/artifact-test.f` — registry interning/equality/key round-trip/count
  regressions, the private-mint unforgeability negative, and fail-closed empty-key /
  out-of-range-id boundaries.
- `maki/db/artifact.f` — the canonical artifact envelope codec (MODEL-CAD-V2-PLAN.md
  § 23.9): reopens `package ARTIFACT` to add checked `BUILD`/`ENCODE`/`DECODE`/`DIGEST`/
  `VALIDATE` over per-kind `weight-artifact`/`kernel-artifact` handles, the four-word
  `content-digest`, and the `art-result<n>` failure taxonomy (malformed, noncanonical,
  bounds, duplicate, unknown-required, kind-mismatch, unsupported-migration,
  digest-mismatch). Fixed little-endian widths, ascending length-delimited tags,
  canonical ascending/duplicate-free dependency set, opaque retention of unknown
  optional fields, SHA-256 digest over the semantic prefix (excludes the digest and
  the created-event). The five digest-covered foreign identity fields — schema-id,
  producer-id, config-id, numeric-policy-id, target-id — are serialized ACROSS the
  owner package boundary via each owner's total `ID>WIRE` / fail-closed `WIRE>ID`
  (SCHEMA/PRODUCER/CONFIG/NPOL/TARGET); a `WIRE>ID` reject folds into the taxonomy
  (wrong-width→malformed, unknown→bounds). `VALIDATE` is the kind-AGNOSTIC leg (full
  structural + digest check over owned bytes, any known kind, sharing DECODE's core).
  No new trust boundary: handle constructors are checker-native, foreign ids are held
  whole in typed columns (never raw-cast in ARTIFACT), and the only in-package id on
  the wire (CAD-KIND:artifact-id) uses maki/artifact.f's existing private refinements.
  Reserved-but-unwired: capability-id (owner CAP, user-gated) and the journal-minted
  audit-event-id (owner JOURNAL); the process-local raw wire form still awaits the
  cross-process content-key reconciliation (§ 23.9, out of scope).
- `maki/db/artifact-test.f` — envelope acceptance: equal values encode
  byte-identically and hash identically, dependency insertion order is irrelevant,
  one semantic field changes the digest while the excluded created-event does not,
  decode round-trips encode (every field, foreign ids included), unknown-required /
  digest-mismatch / malformed / noncanonical / duplicate / bounds / kind-mismatch /
  unsupported-migration each return the right typed diagnostic, and an unknown optional
  field is retained and re-emitted verbatim. Second slice: each foreign id flips the
  digest (schema/producer/config/npol by a two-id ENCODE flip, target-id via the DECODE
  digest-mismatch path since its 16-slot registry is shared/full in the suite), each
  foreign field surfaces bounds (out-of-range) / malformed (wrong-width) on a bad
  decode, and `VALIDATE` accepts the golden weight AND kernel envelopes (kind-agnostic)
  while rejecting each corruption class.
- `maki/db/transaction.f` — the deterministic transaction data model (MODEL-CAD-V2-PLAN.md
  § 23 "Deterministic transactions" + § 23.1). Package TX: the checked immutable `txn`
  handle (transaction id), base revision (`CAD-KIND:rev-id`), read set WITH negative
  lookups (present/absent per object), write object set, dependency edges, capability
  set, budget ledger, and obligations, all canonically ordered by identity and
  deduplicated at `BUILD`. `VALIDATE` returns the `tx-result<n>` sum (duplicate-write /
  omitted-read rejects; ok otherwise); `IDEMPOTENCY-KEY` is a SHA-256 `idem-key` over the
  full canonical action (stable across retries); `PROPOSE` is the commit proposal - the
  proposed `CAD-KIND:rev-id` via `REV:COMMIT` over a digest of (base + write set), so
  replay reproduces the rev-id; `ENCODE`/`DECODE` round-trip the canonical bytes,
  rehydrating objects via `ARTIFACT:REGISTER`, the base via `REV:WIRE>KEY`, and obligations
  via `OBLIG:WIRE>KEY`. Objects use `CAD-KIND:artifact-id`; obligations are interned
  `CAD-KIND:obligation-id` (maki/db/obligation.f) canonically ordered by 32-byte content
  key; capabilities stay closed-vocabulary codes until the CAP owner lands. `ENCODE-REV`
  emits the canonical revision content (base + write set) and `IDEM-KEY>WIRE` the 32-byte
  idempotency digest, for the durable commit store. `CAP-MASK@` (declared capability codes
  folded to a u64 bitmask) and `BUDGET-AT@` (the k-th declared dimension code + amount) are
  additive read accessors the authorized-commit gate reads (dot habu-v2-capability-and-0970a96d).
  Throws only at capacity / missing-base. Owns -5350..-5353.
- `maki/db/transaction-test.f` — the § 23 acceptance: canonical round-trip and
  insertion-order independence, duplicate/conflicting writes reject, an omitted read
  dependency rejects validation, retry identity is stable (same logical txn -> same
  idempotency key; polarity and base revision are part of the key), the commit proposal
  is deterministic, every field round-trips at its cardinality, truncated bytes decode
  malformed, and capacity/polarity/no-base throw. Obligations are interned via `OBLIG:`.
- `maki/db/commit-store.f` — the crash-safe transaction commit slice over a MINIMAL
  file-backed object store (MODEL-CAD-V2-PLAN.md § 23 "Commit atomically publishes ...;
  recovery either observes the old revision or the complete new revision"; V2-2 exit).
  Package CSTORE: one file per revision (revs/&lt;revhex&gt;, content-addressed via
  `REV:KEY>WIRE`), a single HEAD commit marker advanced by one atomic rename (the
  linearization point), and a commits/&lt;idemhex&gt; idempotency record. `COMMIT` validates
  (composing `TX:VALIDATE`), honours idempotency, rejects a stale base/head with the typed
  `commit-result` `conflict`, then stages the revision, advances the marker, and records
  the idempotency key. The three publish steps `STAGE-REV`/`ADVANCE-HEAD`/`WRITE-IDEM` are
  public and are the crash-injection surface (no failpoint branch in `COMMIT`);
  `HEAD-IS?`/`REV-COMPLETE?` let a fresh process assert recover-old-or-complete-new. Uses
  the `ATOMIC-WRITE-FILE`/rename discipline; fsync/dir-sync is a missing native capability
  (process-crash safe, not power-loss durable). `COMMIT-AUTHORIZED` (dot
  habu-v2-capability-and-0970a96d) is the capability + budget GATED commit: it rejects
  `auth-result` `unauthorized` (granted authority ⊉ the txn's declared caps) and `exhausted`
  (declared reserve ⊄ the ledger remaining, naming the dimension) BEFORE any publish, then
  delegates to `COMMIT` and charges the ledger exactly once per idempotency key on a fresh
  publish (a retry / stale-head / validation reject charges nothing). `COMMIT-DISCHARGED` (dot
  habu-v2-deterministic-audit-428d27c2) threads `DAUTH:AUTHORIZED-DISCHARGE` as the THIRD
  validate leg: it takes new obligation + discharge-evidence + authority parameters, runs the
  folded verifier-class / independence / identity-allowlist gate FIRST (a non-discharge is
  `commit-discharge-result` `not-discharged`, an off-allowlist verifier `unauthorized-verifier`,
  both before any publish or charge), records the successful decision as a canonical
  `AUDIT:RECORD-EVIDENCE-DECISION` event, then delegates to the shared capability + budget
  publish (`AUTHORIZED-PUBLISH`, single-sourced with `COMMIT-AUTHORIZED`). Owns -5371..-5373.
- `maki/db/commit-store-test.f` — in-process acceptance against a real private store:
  deterministic replay yields an equal revision digest, idempotent retry returns the
  original result, a stale head returns the typed conflict, a duplicate write rejects, and
  crash injection at every publish boundary (before-rev / after-rev / after-head) never
  exposes a partial revision.
- `maki/db/commit-store-crash-child.f` — package CSCRASH: the fresh-process crash side.
  `RUN-PARTIAL` runs a PREFIX of the real publish sequence against a shared store and lets
  the process die at that boundary. Shared content-addressed fixtures make the child's
  proposed revision key equal the parent's.
- `maki/db/commit-store-crash-test.f` — the DECISIVE cross-process crash + recovery test:
  the parent writes a genesis head, spawns a fresh bin/hb that crashes after the object
  stage (recovery sees OLD) or after the marker advance (recovery sees the COMPLETE NEW
  revision), and asserts the invariant by content key. Spawn/capture is the
  keywire-xproc-env fresh-process pattern.
- `maki/db/commit-store-discharge-test.f` — acceptance for the folded obligation-discharge
  third leg `CSTORE:COMMIT-DISCHARGED` (dot habu-v2-deterministic-audit-428d27c2): an
  authorized discharge commits and records ONE evidence-decision audit event; a non-discharging
  evidence is `not-discharged` (the discharge leg fires BEFORE the capability leg) and an
  off-allowlist verifier `unauthorized-verifier`, both leaving HEAD unchanged, charging nothing,
  and recording no event; the capability + budget legs still gate a discharged commit.
- `maki/db/audit-log.f` — the canonical, content-chained audit EVENT log + deterministic replay
  (MODEL-CAD-V2-PLAN.md § 23.9 "append-only audit records and deterministic replay"; dot
  habu-v2-deterministic-audit-428d27c2). Package AUDIT: eight typed event kinds (action
  request/result, txn-commit, verifier-run, evidence-decision, promotion, activation, rollback)
  each recorded as a fixed 131-byte canonical record wired to the landed identities by their
  CROSS-PROCESS content keys (`REV:KEY>WIRE` / `TX:IDEM-KEY>WIRE` / `ARTIFACT:KEY>WIRE` /
  `EVIDENCE:KEY>WIRE` / `PRODUCER:KEY>WIRE`) and CONTENT-CHAINED (each record embeds the SHA-256
  of the previous). `VERIFY-LOG` rejects tamper / reorder / omission (typed `verify-result`
  `broken-chain` / `bad-head`) and a marked nondeterministic record stripped of its captured-output
  key (`bad-nondeterministic`). `STATE-DIGEST` is the byte-stable replay: a rolling fold over a
  serialized log frame that uses the CAPTURED key for a nondeterministic event and the primary key
  for a deterministic one, invoking no chooser/registry, so it reproduces identically in any
  process. Self-contained store (distinct from `maki/journal.f`'s occurrence journal, which
  promotion also appends to). Owns -5614..-5615.
- `maki/db/audit-log-test.f` — in-process acceptance: a 6-event log verifies; a mid-record tamper
  is `broken-chain` at the next index and a last-record tamper `bad-head`; a record swap and a
  dropped middle record are `broken-chain`; a nondeterministic captured verifier run verifies and
  its captured key (not the live output) drives the replay digest; a stripped capture is
  `bad-nondeterministic`; a static fixture proves a marked verifier run cannot be recorded without
  a captured id; re-recording against a fresh store reproduces the digest without a chooser;
  overflow / small-buffer fail closed.
- `maki/db/audit-log-xproc-child.f` — package AUDIT-XPROC: the fresh-process replay side. Shared
  `BUILD-LOG` records the canonical event sequence from content-addressed identities (no store);
  `RUN-CHILD` reads the parent's serialized frame, `VERIFY-LOG`s it, then rebuilds the same log
  from an EMPTY store under a decoy-shifted registry and prints XPROC-OK iff the rebuild is
  byte-identical.
- `maki/db/audit-log-xproc-test.f` — the DECISIVE cross-process byte-stability test: the parent
  serializes the log to a file and spawns a fresh bin/hb (decoy-shifted) that rebuilds a
  byte-identical frame, proving replay is byte-stable across processes because events are keyed by
  cross-process content keys. Spawn/capture is the keywire-xproc fresh-process pattern.
- `maki/db/diagnostic.f` — the common structured Diagnostic IR (MODEL-CAD-V2-PLAN.md
  § 23.9 "Structured failure IR" + § 23.2): one `diagnostic` handle every checker /
  compiler / pass / runtime / benchmark / deployment / policy failure lowers to, plus
  its canonical wire codec (`ENCODE`/`DECODE`). The nine failure classes are the closed
  `DIAG:class` ENUM (invariant, unsupported, invalid-input, resource, external, numeric,
  performance, stale-evidence, authorization); `severity`, `phase`, and `repair` are the
  same closed-ENUM substrate. Common fields: code, class, severity, owner, subject,
  revision, phase, location, expected/observed facts, dependency cone, counterexample,
  legal repairs, invalidated evidence, reproduction, environment, parent, progress. The
  staged builder (`NEW` / typed setters / `BUILD`) returns a custom-sum `build-result`;
  a diagnostic with no owner or no reproduction is rejected typed (`missing-owner` /
  `missing-reproduction`), never a throw. Identity fields use the landed nominal ids via
  their owner packages (owner→producer-id, environment→config-id, revision→rev-id,
  subject/counterexample/dependency-cone→artifact-id, serialized by each owner's public
  codec, never a raw cast);
  DECODE folds a foreign-id reject into the taxonomy (wrong-width→malformed,
  unknown→bounds). Fixed little-endian widths, ascending length-delimited tags, custom-sum
  `decode-result` taxonomy (malformed, noncanonical, bounds, duplicate, unknown-required).
  CONSERVATIVE READINGS flagged at the definition site: subject modeled as artifact-id
  (content-digest is ARTIFACT-private), environment as config-id, invalidated-evidence as
  strings (evidence-id has no owner registry yet), parent as the parent's code, location /
  facts as strings. Owns -5354..-5358.
- `maki/db/diagnostic-render.f` — the two renderers over one value: `DIAG:RENDER`
  (human text) and `DIAG:RENDER-JSON` (canonical JSON via the checked lib/json-write.f
  builder, no host tooling). Both consume the same `diagnostic` handle through the typed
  accessors and share the single-source enum NAME words, so they never disagree on a
  label. No construction or mutation surface.
- `maki/db/diagnostic-test.f` — Diagnostic acceptance: a rich everything-set diagnostic
  round-trips byte-identically and field-for-field; missing owner / missing reproduction
  reject typed; both renderers consume the same value (text + JSON substrings); four
  realistic repo failure classes lower losslessly (A checker reject, B ptxas failure,
  C device-launch fault, D gate timeout), each proven byte-identical and field-for-field;
  and the decode-result reject taxonomy (malformed, noncanonical, duplicate, bounds,
  unknown-required) is reachable and typed.
- `maki/db/obligation.f` — the typed proof-obligation schema (MODEL-CAD-V2-PLAN.md
  § 23.9 "Proof obligations and independent verifiers", plan:3737-3755). Package OBLIG:
  the immutable `obligation` value naming a subject (`CAD-KIND:artifact-id`), a claimed
  `relation`, a proof `domain`, an `independence` policy, a `verifier` class, a required
  environment (`CAD-KIND:config-id`), and the proposing producer (`CAD-KIND:producer-id`)
  plus its dependency cone. The six proof domains (exact, approximate, empirical, device,
  safety, performance) are one closed DERIVE-eq ENUM with NO coercion lattice (contrast
  maki/numpolicy.f NPOL:dom, the numeric-equivalence strength lattice it PROJECTS from via
  the total one-way `NPOL>DOMAIN` bridge: exact→exact, ulp/relative→approximate,
  empirical→empirical; device/safety/performance have no numeric source). `DISCHARGE`
  is the named-field gate returning the `discharge-result` sum (wrong-subject / wrong-domain
  / wrong-relation / wrong-environment / wrong-verifier-class / not-independent / ok);
  `INVALIDATED-BY?` invalidates exactly the affected obligation (subject or dependency-cone
  member). Canonical `ENCODE`/`DECODE` (ascending length-delimited tags, LE widths,
  `decode-result` taxonomy) serialize the subject and dependency cone as artifact `KEY$`
  and the environment / producer as their owner cross-process content keys (CONFIG /
  PRODUCER `KEY>WIRE`, fail-closed `WIRE>KEY`). Conservative readings flagged at the
  definition site (subject as artifact-id, environment as config-id, policy as the
  independence governance only, relation/verifier closed vocabularies, producer as the
  claimant). Also the content-addressed owner registry for `CAD-KIND:obligation-id`:
  `INTERN` interns an obligation by its canonical ENCODE bytes (equal obligations share one
  id) plus `ID-EQUAL?` / `ID-VALIDATE` / `ID-COUNT` and two wire codecs — `ID>WIRE`/`WIRE>ID`
  (8-byte process-local raw) and cross-process `KEY>WIRE`/`WIRE>KEY` (32-byte SHA-256 of the
  canonical encoding). Private `RAW>OBLIGATION-ID` / `OBLIGATION-ID>RAW` refinements are the
  only trust boundary. Owns -5359..-5365.
- `maki/db/obligation-test.f` — proof-obligation acceptance, each plan rule by a named
  test: wrong-domain evidence cannot discharge (a typed `wrong-domain` reject plus the
  cad-kinds verdict-fixture static leg — the checker rejects a `relation` or raw int where
  a `domain` is required); subject / environment / relation / verifier-class mismatch
  rejects; the producer cannot be the sole verifier under an INDEPENDENT policy while a
  SELF-VERIFY policy permits it; a changed dependency invalidates the affected obligation
  and an UNRELATED obligation survives; the NPOL:dom→domain projection; a byte-identical +
  field-for-field round-trip; and the decode reject taxonomy including the foreign-id fold.
  Also the obligation-id registry: content-addressed interning (equal obligations one id),
  the `ID>WIRE`/`WIRE>ID` and cross-process `KEY>WIRE`/`WIRE>KEY` round-trips, the SHA-256
  content-key identity, fail-closed decode (wrong-width + unknown), and range-checked mints.
- `maki/db/evidence.f` — the evidence-descriptor identity registry + wire codec (the
  evidence-id leg of MODEL-CAD-V2-PLAN.md § 23.9; dot habu-v2-evidence-applicability-73ac58b9).
  Package EVIDENCE: `EVIDENCE:REGISTER` interns the canonical evidence DESCRIPTOR bytes to a
  content-addressed `CAD-KIND:evidence-id` (equal descriptors share one id), plus
  `DESCRIPTOR$`/`EQUAL?`/`VALIDATE`/`COUNT` and two codecs — `ID>WIRE`/`WIRE>ID` (8-byte raw)
  and the cross-process `KEY>WIRE`/`WIRE>KEY` (32-byte SHA-256 content key over the interned
  descriptor). The durable identity the diagnostic IR's `invalidated-evidence[]` promotes to;
  a DIFFERENT concern from the proof-EVIDENCE value (package OBLIG) and the evidence CLASS
  families (package EVID). Private `RAW>EVIDENCE-ID` / `EVIDENCE-ID>RAW` refinements are the
  only trust boundary. Owns -5366..-5369.
- `maki/db/evidence-test.f` — evidence-id acceptance: content-addressed interning (equal
  descriptors one id, distinct descriptors distinct id), the `ID>WIRE`/`WIRE>ID` and
  cross-process `KEY>WIRE`/`WIRE>KEY` round-trips, the SHA-256 content-key identity,
  fail-closed decode (wrong-width + unresolved raw), cross-role rejection, and the
  private-mint unforgeability negatives.
- `maki/db/evidence-applicability.f` — obligation closure + evidence applicability
  (MODEL-CAD-V2-PLAN.md § 23.9; dot habu-v2-evidence-applicability-73ac58b9). Package APPLIC
  composes the landed `OBLIG:DISCHARGE` / `INVALIDATED-BY?` primitives over a tracked
  obligation set, an available-evidence pool, and a change-set into `VERDICT` — the typed
  `applicability` sum (applicable / stale / missing / inapplicable): `stale` when discharged
  evidence is invalidated by a changed subject/dependency, `inapplicable` when evidence is
  about the subject but a non-subject axis rejects (the home of "static proof cannot satisfy
  device execution" and "performance cannot satisfy equivalence"), `missing` when no evidence
  is even about the subject. `INVALIDATED-SET-UNCACHED` / `INVALIDATED-SET-CACHED` compute the
  minimal invalidation set (a u64 bitmask over the tracked slots) directly and via a reverse
  dependency-index cache; `CLOSURE-EQUAL?` proves they agree. No new trust boundary; owns -5370.
- `maki/db/evidence-applicability-test.f` — the closure/applicability acceptance: the
  mutation matrix (mutating each of subject / domain / relation / environment / verifier-class /
  verifier-identity flips the verdict to exactly the affected result), the two structural
  refusals (static-vs-device, performance-vs-equivalence, each with a discharging positive
  control), and the minimal invalidation set per change-set with cache-equals-uncached proven.
- `maki/db/promotion-policy.f` — the promotion POLICY value + content digest (dot
  habu-v2-evidence-promotion-f8312ebe). Package PPOLICY: the immutable `spec` product BINDS
  ten typed fields over the landed § 23.9 identities — model, weights (`CAD-KIND:artifact-id`),
  target (`target-id`), numeric policy (`numeric-policy-id`), populations (`config-id`,
  conservative reading), verifier identity (`producer-id`) + version, threshold, expiry, and
  rollback artifact. `DIGEST-WORDS` is SHA-256 over the canonical serialization (each identity
  as its owner `KEY>WIRE` content key, each scalar LE64) as four words — the digest a promoted
  value carries, so any changed bound field digests differently; `DIGEST-EQ?` and `BIND` (the
  single-cell model/expiry/digest projection `PROMOTE:SATISFY` consumes) round it out. Owns -5606.
- `maki/db/promotion-policy-test.f` — policy acceptance: field-identical policies share a
  digest; a changed threshold / expiry / verifier-version each digests differently; `BIND`
  yields the model, expiry, and the digest words matching `DIGEST-WORDS`.
- `maki/db/promotion-authority.f` — the folded obligation-discharge AUTHORITY gate (dot
  habu-v2-evidence-promotion-f8312ebe, the discharge-authority leg). Package DAUTH: a sealed
  `authority` (an allowlist of authorized verifier `producer-id`s, minted via NEW/AUTHORIZE+/
  SEAL, the CAPTOK:ROOT discipline) and `AUTHORIZED-DISCHARGE` — the typed `authz-result`
  (ok / not-discharged / unauthorized) that FOLDS the three authorization legs: verifier CLASS
  and INDEPENDENCE via the landed `OBLIG:DISCHARGE`, plus verifier IDENTITY via the allowlist.
  The reusable third leg `CSTORE:COMMIT-AUTHORIZED` needs (wiring it there is a separate change:
  the transaction carries no obligation/evidence discharge context yet). Owns -5607..-5608.
- `maki/db/promotion-authority-test.f` — authority acceptance: discharges + authorized -> ok;
  discharges but verifier off the allowlist -> unauthorized; wrong subject / wrong verifier
  class / independence-violation (even for an authorized producer) -> not-discharged (the
  discharge refusal wins over authorization).
- `maki/db/promotion.f` — the immutable evidence-promotion TYPESTATE (MODEL-CAD-V2-PLAN.md
  § R7 artifact<promoted> + § 23.9; dot habu-v2-evidence-promotion-f8312ebe). Package PROMOTE:
  Candidate -> Verified -> Measured -> PolicySatisfied -> Promoted, each a distinct sealed
  product DERIVED (never mutated) from the prior — every stage carries a class-private proof
  token whose PRIVATE mint makes a raw n / wrong-stage value unable to forge it (the static
  unconstructibility leg). `VERIFY` / `MEASURE` mint the next stage ONLY when the obligation is
  APPLICABLE over the session working set (compose `APPLIC:VERDICT`); missing / stale /
  wrong-target evidence makes the verdict non-applicable and the transition refuses
  (`E-PROMO-UNAPPLICABLE`), so the stage is unconstructible without applicable evidence.
  `SATISFY` binds the policy digest (and enforces model + expiry), threaded into `PROMOTE`,
  whose `REVALIDATE` fails a changed policy (digest-bound). `PROMOTE` records the EXACT
  obligation closure (content keys + verdicts) to the journal; `REPLAY-DESC$` (recorded) equals
  `CLOSURE-DESC$` (recomputed). Owns -5609..-5613.
- `maki/db/promotion-test.f` — typestate acceptance: the typed reject (APPLIC verdict for
  present/changed/wrong-target/wrong-domain evidence), the constructor refusal (VERIFY/MEASURE
  throw), the static leg (raw-n / wrong-stage rejected verdict 0, private mint unresolvable
  verdict 1 / search-wl), identity threading unchanged Candidate->Promoted, policy-change
  invalidation both directions, and recorded-equals-recomputed audit closure.
- `maki/db/action.f` — the machine-facing action-schema registry (MODEL-CAD-V2-PLAN.md
  § 23.9 "Machine-facing action registry", plan:3825; dot habu-v2-machine-action-a7357409).
  Package ACTION owns `CAD-KIND:action-id` and interns each callable protocol action by
  canonical NAME (the maki/producer.f precedent) with a full typed declaration: checked
  input/output `art-kind`, preconditions, effects, capabilities (opaque closed-vocabulary
  bit CODES — the user-gated vocabulary is not invented here; seeded EMPTY), deterministic/
  cacheable flags, budget dimensions, produced obligations (reused `OBLIG:relation`),
  verifier (reused `OBLIG:verifier`), diagnostics (reused `DIAG:class`), and invalidation —
  each set held as a canonical-by-construction u64 bitmask. `REGISTER` rejects an incomplete
  declaration (typed) and a name re-registered with a different declaration (conflict);
  `DISPATCH` is the protocol GATE (never the executor), returning `wrong-kind` /
  `unauthorized` (declared effects+caps ⊄ granted) / `unsupported` (a `declared`, not-yet-
  implemented action) / `unknown-action` / `accepted`. `ENUM-AT` / `DIGEST` enumerate
  name-sorted and digest deterministically, so registrations REPLAY to one enumeration.
  Seeds SCHEMA:LIST, ARTIFACT:GET, REVISION:DIFF, TX:BEGIN/APPLY/VALIDATE/COMMIT/ABORT,
  PASS:RUN — `implemented` where a landed surface realizes the op, else `declared`. Owns
  -5374..-5379.
- `maki/db/action-test.f` — action-registry acceptance: missing field -> REGISTER
  incomplete (typed) with an idempotent/conflict control; wrong input kind cannot dispatch
  STATICALLY (a non-kind in the DISPATCH kind slot is verdict 0, a real art-kind verdict -1
  — the cad-kinds verdict pattern) PLUS the dynamic wrong-kind reject; unauthorized effects
  and capabilities reject before execution; staged availability (declared -> unsupported,
  implemented -> accepted); registry enumeration is canonical (name-ascending) and
  REPLAYABLE (the same set in reverse order digests identically); and the seeded
  declarations reflect the landed surfaces.
- `maki/db/diff-suite.f` — the DifferentialSuite artifact schema (MODEL-CAD-V2-PLAN.md
  § "Automatic differential verification", plan:3782-3796; dot
  habu-v2-differential-suite-2d896ced). Package DIFFSUITE: an immutable content-addressed
  suite naming deterministic generators/corpora (content-key sets), independent reference
  producers (`CAD-KIND:producer-id` set), normalization + minimizer descriptors (content
  keys), comparison domain/tolerance, metamorphic properties (content-key set), target
  needs (`CAD-KIND:target-id`), seed, and the reused `BUDGET:dim` vector. The comparison
  domain COMPOSES with `NPOL:dom` (held as `CAD-KIND:numeric-policy-id`; exact -> zero
  tolerance, approximate -> positive tolerance); the independence policy REUSES
  `OBLIG:independence` (under `independent` a reference may not alias the subject producer,
  by content-key = `PRODUCER:EQUAL?`). `SEAL` returns a typed `build-result`
  (incomplete / tolerance-mismatch / reference-not-independent / ok). `DIGEST-INTO` is
  SHA-256 over the canonical semantic prefix (every field digest-covered, the envelope
  precedent); `ENCODE` appends the stored digest; `CASE-ID(suite,k)` = SHA-256(digest ||
  seed || k) for deterministic replay. Owns -5392..-5394.
- `maki/db/diff-suite-test.f` — DifferentialSuite acceptance: a per-field digest FLIP
  MATRIX (every one of the twelve semantic fields flips the digest; identical suites and
  permuted set order hash equally); incompatible comparison domain/tolerance pairs REJECT
  typed (exact+nonzero, ulp/relative+zero -> tolerance-mismatch) with compatible controls;
  a reference cannot alias the subject under `independent` (reference-not-independent) with
  the `self-verify` and distinct-reference positive controls; deterministic case-id replay
  (same suite/any order -> identical case-id sequence, different seed -> different); the
  canonical envelope round-trips byte-identically and carries the DIGEST-INTO tail; and the
  static leg (nominal id families are checker-guarded).
- `maki/experiment/run.f` — the immutable experiment-run identity (MODEL-CAD-V2-PLAN.md
  § 23.4, plan:3300-3317; dot habu-v2-experiment-run-7c1d1906). Package RUN owns
  `CAD-KIND:run-id`: a staged builder (`NEW` + typed setters for seed/rng/dataset/split/
  preprocess/model/optimizer/numeric/target/compiler/environment/license/authority) whose
  `SEAL` canonically serializes the thirteen digest-covered fields, SHA-256-digests them, and
  INTERNS the digest so two equal builds yield ONE run-id (equal keys resume one identity).
  Fields are typed over the landed § 23.9 identities where owners exist (artifact-id for
  dataset/model, numeric-policy-id, target-id, config-id for compiler+environment) and
  content keys where none do (rng/split/preprocess/optimizer/license/authority); the
  compiler->config-id and license/authority->content-key readings are conservative + flagged
  at the definition site. `SEAL` rejects `incomplete` on a missing field (license/authority
  included). `KEY>WIRE`/`WIRE>KEY` are the durable content-key codec; `EQUAL?` is same-run
  identity; `BATCH-ID(run,k)` = SHA-256(run-key||k) is the deterministic next-batch id. Owns
  -5616..-5619.
- `maki/experiment/run-test.f` — run-identity acceptance: a per-field digest FLIP MATRIX
  (every one of the thirteen semantic fields flips the run digest; identical keys hash
  equally); intern (equal keys one id, a changed field a distinct id); the cross-process
  content-key round-trip + fail-closed decode (wrong-width, unknown); missing license /
  authority / empty -> `incomplete` with a complete-ok control; and deterministic next-batch
  identity (stable across a rebuild + computation order, distinct per index and per run).
- `maki/experiment/run-metric.f` — typed metric POPULATIONS with the train/held-out
  separation (MODEL-CAD-V2-PLAN.md § 23.4, plan:3310-3312; dot
  habu-v2-experiment-run-7c1d1906). Package RUNMETRIC: two DISTINCT nominal families
  (`report-metric` any population, `objective-metric` training-objective-eligible) that never
  unify, so a held-out measurement (a report-metric) passed to the objective consumer
  `AS-OBJECTIVE` is a compile-time reject (held-out-as-objective STATICALLY untypeable). The
  sole bridge `PROMOTE-OBJECTIVE` refines a report-metric to an objective ONLY for a `train`
  population, else the `not-training` reject (dynamic leg). `MEASURE` records a metric over
  the population/direction/aggregation enums; `COMPARABLE?` forbids comparing unlike
  populations. Units (the fourth plan axis) is a documented follow-up. Owns -5620.
- `maki/experiment/run-metric-test.f` — metric acceptance: the STATIC verdict fixtures
  (report-metric -> AS-OBJECTIVE rejects, objective-metric certifies; the families never
  unify either way; PROMOTE-OBJECTIVE takes report-metric only) and the DYNAMIC leg (train
  promotes to an objective, held-out/validation reject not-training); the objective carries
  its scalar + direction; COMPARABLE? forbids unlike populations/directions.
- `maki/experiment/run-lineage.f` — the per-run lineage log keyed by run identity, composing
  with the append-only journal (MODEL-CAD-V2-PLAN.md § 23.4, plan:3296-3298; dot
  habu-v2-experiment-run-7c1d1906). Package RLINEAGE: `LINEAGE+` records an event through
  `JOURNAL:APPEND` (a fresh monotonic audit-event-id per append) and buckets it by the run's
  cross-process content key (`RUN:KEY>WIRE`), so equal run keys - even across a resume - share
  one lineage; `LINEAGE-COUNT`/`LINEAGE-AT` read the ordered, typed events. Owns -5621.
- `maki/experiment/run-lineage-test.f` — lineage acceptance: equal keys resume one lineage
  (append across a rebuild -> LINEAGE-COUNT 2, the rebuild is the same interned id); lineage
  composes with the journal (two appends -> two distinct audit events); LINEAGE-AT returns
  each event in order; a different run key has an independent lineage.
- `maki/db/diff-runner.f` — the differential runner CORE (MODEL-CAD-V2-PLAN.md § "Automatic
  differential verification", plan:3787-3796; dot habu-v2-differential-runner-13359019).
  Package DIFFRUN: over a sealed `DIFFSUITE:suite`, executes cases deterministically through
  typed defer adapters (`SUBJECT-RUN ( n -- run-result )` / `REFERENCE-RUN ( n -- ref-result )`,
  installed with `SUBJECT!` / `REFERENCE!`), compares subject vs reference under the suite's
  declared `NPOL:dom` + tolerance (`CLOSE?`), and classifies each case into a `case-verdict`
  (agree / mismatch / subject-fault / reference-skip) where a fault DOMINATES so a hung/dying
  subject is never a wrong value. `CLASSIFY-OUTCOME` is the spawn-taxonomy boundary (a
  `lib/process.f` `outcome`: only exit(0) is produced-eligible). `RUN` loops cases into a
  content-addressed run-log and returns a `run-verdict` by first-failure order; `MINIMIZE`
  shrinks a discrepancy to its minimal counterexample (pure function; original preserved as a
  distinct `CASE-ID`); `EMIT-EVIDENCE` interns success evidence keyed by subject-key ||
  suite-digest || environment-key (each flip distinct, via `EVIDENCE:REGISTER`);
  `EMIT-COUNTEREXAMPLE` lowers a discrepancy to a lossless `DIAG:diagnostic`. Consumes
  DIFFSUITE / EVIDENCE / DIAG, never forks them. Owns -5395..-5398.
- `maki/db/diff-runner-test.f` — runner CORE acceptance (scalar checker suite, exact domain):
  (a) an injected mismatch MINIMIZES to its minimal counterexample and replays deterministically
  (minimizer is a pure function, minimized case is a distinct content-addressed artifact, the
  original is untouched); (b) `CLASSIFY-OUTCOME` maps timeout/signal/nonzero-exit to fault and
  only exit(0) to produced, and a scripted fault at an otherwise-agreeing case grades
  subject-fault DISTINCT from a genuine mismatch; (c) a reference-unavailable leg records a
  reference-skip, not a mismatch; (d) success evidence is subject/suite/environment keyed (each
  flip distinct, equal triple = one id); and the counterexample DIAG round-trips losslessly.
- `maki/db/diff-runner-spawn.f` — the SPAWN-ISOLATED subject adapter + external-process PyTorch
  reference adapter (reopens package DIFFRUN). `SPAWN-SRC` / `SPAWN-CASE` run each case in a
  FRESH spawned `bin/hb` child (the grader pattern, `maki/eval/device.f`): the child prints its
  scalar and completes naturally (exit 0 = success, not `bye`); the parent captures the outcome
  and classifies through `CLASSIFY-OUTCOME`, so a crashing/hanging subject is a graded fault,
  never a grader casualty. `TORCH-REFERENCE` is the external torch reference INTERFACE
  (`ort-ref` pattern): off-device / without `DIFFRUN_TORCH` it records a SKIP, keeping the
  reference outside Habu semantics.
- `maki/db/diff-runner-spawn-test.f` — REAL spawn-isolation acceptance (b): actual `bin/hb`
  children that produce a scalar (produced), HANG (timeout, SIGKILL-reaped -> faulted), and DIE
  nonzero (faulted); the shipped `SPAWN-CASE` adapter produces the case scalar in an isolated
  child; and (c) the off-device torch reference records a skip. The real-process complement to
  the deterministic classifier proof in `maki/db/diff-runner-test.f`.
- `maki/db/budget-dim.f` — the closed budget-dimension vocabulary (MODEL-CAD-V2-PLAN.md § 23
  autonomy resource budgets, plan:3205-3211; dot habu-v2-capability-and-0970a96d). Package
  BUDGET owns ONE variant-exhaustive `dim` ENUM (compute-time, device-time, storage,
  candidate-count, retries, external-effects) with a stable `DIM>N` wire ordinal, a fail-closed
  `N>DIM` inverse, and `DIM-COUNT`. The SINGLE source shared by the capability grant's budget
  vector and the ledger's remaining vector, so the two never fork a competing dimension set.
- `maki/db/budget-dim-test.f` — budget-dim acceptance: `DIM>N`/`N>DIM` round-trip over all six
  dimensions, `DIM-COUNT`, and an out-of-domain ordinal failing closed (E-BUDGET-DIM).
- `maki/db/capability.f` — the finite, UNFORGEABLE capability GRANT token (MODEL-CAD-V2-PLAN.md
  § 23 autonomy authority + § 23.1 capability set/tokens, plan:3203-3235; dot
  habu-v2-capability-and-0970a96d). Package CAPTOK: a grant is a nominal handle (`CAPTOK:grant`)
  over an append-only authority slot storing an opaque capability-code BITMASK (the user-gated
  CAP vocabulary is NOT invented here) plus a budget-ceiling vector. Refined via the private
  TRUSTED `RAW>GRANT`/`GRANT>RAW` pair (the RAW>ACTION-ID precedent), so a raw n cannot forge a
  grant. `ROOT` is the authority origin; `ATTENUATE` derives a child that is provably a SUBSET on
  both axes (`escape-cap` / `escape-budget` name an over-reach), so nested actions cannot exceed
  parent authority. `AUTHORIZES?`/`COVERS?` are the ACTION:DISPATCH containment gates. Owns -5381..-5382.
- `maki/db/capability-test.f` — grant acceptance: STATIC forge reject (a raw n where a grant is
  required is verdict 0; the sealed `RAW>GRANT` is unresolvable verdict 1; `=` on a grant rejects)
  plus dynamic ROOT/ATTENUATE subset-accept, both-axis escape rejects, transitive (grandchild)
  subset, the AUTHORIZES?/COVERS? gates, and capacity fail-closed.
- `maki/db/budget-ledger.f` — the monotonic budget LEDGER (MODEL-CAD-V2-PLAN.md § 23.1 resource
  budgets, "a failed transaction publishes nothing", plan:3234-3241; dot
  habu-v2-capability-and-0970a96d). Package LEDGER: a pooled-slot handle with a limit + remaining
  vector (monotonic non-increasing) and a charged idempotency-key set. `RESERVE` is a pure typed
  fit check (`exhausted` names the dimension); `CHARGE` deducts atomically and IDEMPOTENTLY keyed
  by a 32-byte idempotency key (a retry never double charges); `DIGEST` content-addresses the
  canonical state so replaying charges in ANY order digests identically. Owns -5385..-5388.
- `maki/db/budget-ledger-test.f` — ledger acceptance: RESERVE fit/exhaust (pure, no mutation);
  CHARGE deduction, idempotent no-double-charge, exhaustion with no deduction and no key; the
  both-order replay digest (equal charges any order -> equal digest, and a differing set differs);
  and pool/charged-key capacity fail-closed.
- `maki/db/commit-store-auth-test.f` — acceptance for `CSTORE:COMMIT-AUTHORIZED` against a real
  private store: empty-authority pass-through; unauthorized (granted ⊉ declared caps) with HEAD
  unchanged; authorized commit + HEAD advance + charge-exactly-once; idempotent retry with no
  double charge and the same revision; exhaustion (typed, dimension-named, HEAD unchanged, no
  charge); and a duplicate-write reject charging nothing.
- `maki/db/capbud-test.f` — the aggregate maki/test.f suite for the capability + budget subsystem:
  one wired entry that runs the four standalone concern suites (budget-dim, capability, budget-
  ledger, authorized commit), keeping one concern per test file within the maki suite-table budget.
- `maki/db/agent-loop.f` — the bounded autonomous agent-loop CONTROLLER (MODEL-CAD-V2-PLAN.md § 23.2
  repair loop + § 23.9 implementation-order item 8 "Agent-loop controller with bounded progress and
  deterministic replay", plan:3255-3272 / plan:3852; dot habu-v2-bounded-autonomous-1c598fcf). The
  capstone that COMPOSES the landed substrate, forking none of it. Package ALOOP: `RUN` drives an
  UNTRUSTED chooser (a checked `[ -- n txn ]` quotation proposing an action ordinal + txn) under a
  ROOT grant ATTENUATEd to a child + a budget LEDGER, gating every proposal through ACTION:DISPATCH
  and committing the sole mutation via CSTORE:COMMIT-AUTHORIZED; a trusted metric quotation (canonical
  `APPLIC-SATISFIED`, counting tracked obligations whose APPLIC:VERDICT is applicable) measures
  progress. Three bounds (iterations, consecutive non-progress, budget) guarantee termination with a
  typed `loop-result` (`promoted` / `blocked` naming a `blocked-reason`); committed decisions are
  journaled by idempotency key for deterministic replay + crash/retry idempotency. Owns -5389..-5391.
- `maki/db/agent-loop-test.f` — controller acceptance against a real private store + the landed
  substrate: promote-on-progress and the APPLIC focused-verifier path; the untrusted-chooser static
  verdict fixtures (a missing/raw-n txn slot is a checker reject); registry-gate rejects (out-of-range
  ordinal, declared/wrong-kind action -> blocked(illegal-action), no mutation); non-progress and
  iteration bounds terminate; authority + budget hold (DISPATCH-effect / commit-capability / ledger-
  exhaustion / attenuation-escape -> typed blocked, HEAD unchanged); crash/retry idempotency (charge
  once, HEAD stable); replay-without-the-chooser (journal key + HEAD + ledger digest identical); and
  the tracked-obligation / journal capacity throws.
- `maki/db/keywire-xproc-child.f` — the FRESH-PROCESS decode side of the cross-process
  content-key identity test (dot habu-wire-content-key). Package KWXPC: shared fixed
  key-file layout, shared real descriptors, per-family `REG-*` registrations, and
  `RESOLVE-ALL` - loaded by a spawned bin/hb that registers DECOYS FIRST (shifting raw
  indices), reads the parent's key file, and `WIRE>KEY`-resolves every family's content
  key BY CONTENT, printing `XPROC-OK` iff each decoded id projects to its descriptor.
- `maki/db/keywire-xproc-test.f` — the DECISIVE cross-process test: this process
  `KEY>WIRE`-encodes each migrated family's content key into a fixed-layout key file and
  spawns a fresh bin/hb (the child fixture) that decodes them, proving identity survives
  PROCESS DEATH for all seven families (schema, producer, config, rev, target, numeric-
  policy, artifact). The spawn/capture is the maki/cad-test.f fresh-process pattern.
- `maki/db/keywire-xproc-env-child.f` — the fresh-process decode side of the v2
  ENVELOPE + TRANSACTION cross-process test (dot habu-wire-content-key, item-3 leg):
  registers decoys first, reproduces the journal depth (the digest-excluded 8-byte
  event survives only by replay), then DECODEs the parent's envelope and transaction
  bytes, printing `XPROC-OK` iff both decode ok and the txn base revision resolves to
  the same content.
- `maki/db/keywire-xproc-env-test.f` — the v2-migration counterpart of the keywire
  xproc test: BUILDs a v2 envelope (self-id, 2-dep set, foreign ids, source-revision,
  event) and a transaction (base rev + sets), ENCODEs both to a fixed-layout file, and
  spawns the env child — proving envelope identity and the transaction Merkle base
  survive process death under the 32-byte content-key wire forms.
- `maki/numpolicy.f` — the typed numeric-policy proof-domain family (`NPOL:dom` =
  exact/ulp/relative/empirical, MODEL-CAD-V2-PLAN.md §22.6): the strength lattice
  (`RANK`/`SATISFIES?`/`COMPOSE` weakest-wins), the checked satisfaction gate
  `ENFORCE` (E-NPOL-APPROX: approximate evidence cannot satisfy a stricter policy),
  the key token `NAME`, the `DOM>N`/`N>DOM` wire projection, and the
  `NUM>DOM`/`OP-DOM` bridge from op-registry's raw numeric class - the PER-OP axis a
  region's requested policy folds through (maki/sched-key.f REGION-POL), retiring the
  old per-class requested-policy table. Also the `REGISTER` mint and the `ID>WIRE`/
  `WIRE>ID` (8-byte rank) plus `KEY>WIRE`/`WIRE>KEY` codec - numeric-policy is the
  documented § 23.9 exception whose content key IS the 8-byte rank (already cross-
  process-stable), so the cross-process codec coincides with the rank codec.
  Owns -5145..-5147.
- `maki/numpolicy-test.f` — rank ordering, the pinned 4×4 composition table,
  satisfaction, the TF32/GELU/recompute refusal fixtures with positive controls, the
  op bridge, wire roundtrip, and fail-closed throws.
- `maki/schema.f` — the schema-definition identity registry + wire codec (the
  schema-id leg of MODEL-CAD-V2-PLAN.md § 23.9): `SCHEMA:REGISTER` interns the
  canonical, version-independent schema NAME to a content-addressed
  `CAD-KIND:schema-id` (equal names share one id), plus `NAME$`/`EQUAL?`/`VALIDATE`/
  `COUNT` and TWO codecs sharing the private refinement: `ID>WIRE`/`WIRE>ID` (8-byte
  process-local raw) and the cross-process `KEY>WIRE`/`WIRE>KEY` (32-byte SHA-256
  content key over the interned name, § 23.9 origin-class table; resolved by content in
  any process). Retires the former maki/evidence/policy.f `RAW>SCHEMA-ID` placeholder;
  raw conversions stay private (the target.f/artifact.f pattern). Distinct concern from
  maki/evidence/schema.f (package EVID, the evidence-bundle presence schema).
  Owns -5330..-5333.
- `maki/schema-test.f` — content-addressed interning (equal names one id), the wire
  round-trip, fail-closed decode (wrong-width + unresolved raw), cross-role
  rejection, and the private-mint unforgeability negatives.
- `maki/producer.f` — the producer identity registry + wire codec (the producer-id
  leg of MODEL-CAD-V2-PLAN.md § 23.9): `PRODUCER:REGISTER` interns the canonical,
  version-independent producer NAME (a namespaced machine-facing component
  identifier; producer-version is a separate envelope field, class rides the name)
  to a content-addressed `CAD-KIND:producer-id`, plus `NAME$`/`EQUAL?`/`VALIDATE`/
  `COUNT` and two codecs: `ID>WIRE`/`WIRE>ID` (8-byte raw) and the cross-process
  `KEY>WIRE`/`WIRE>KEY` (32-byte SHA-256 content key over the interned name, § 23.9).
  Raw conversions stay private. Owns -5334..-5337.
- `maki/producer-test.f` — content-addressed interning (equal names one id, a
  namespaced class rides the name), the wire round-trip, fail-closed decode
  (wrong-width + unresolved raw), cross-role rejection, and the private-mint
  unforgeability negatives.
- `maki/config.f` — the build/config identity registry + wire codec (the config-id
  leg of MODEL-CAD-V2-PLAN.md § 23.9): `CONFIG:REGISTER` interns the canonical
  build/config FACT STRING - the deterministic toolchain/build facts REMAINING after
  target facts (target-id) and numeric facts (numeric-policy-id) in the plan's
  target/config/numeric three-way split - to a content-addressed `CAD-KIND:config-id`
  (equal fact sets share one id), plus `FACTS$`/`EQUAL?`/`VALIDATE`/`COUNT` and two
  codecs: `ID>WIRE`/`WIRE>ID` (8-byte raw) and the cross-process `KEY>WIRE`/`WIRE>KEY`
  (32-byte SHA-256 content key over the interned facts, § 23.9). The fact vocabulary is
  intentionally open (the boundary, not a closed field set, is the decision); raw
  conversions stay private. Owns -5338..-5341.
- `maki/config-test.f` — content-addressed interning (equal fact sets one id,
  distinct flags distinct id), the wire round-trip, fail-closed decode (wrong-width +
  unresolved raw), cross-role rejection, and the private-mint unforgeability
  negatives.
- `maki/journal.f` — the append-only audit journal + audit-event-id wire codec (the
  audit-event-id leg of MODEL-CAD-V2-PLAN.md § 23.9): `JOURNAL:APPEND` records an event
  descriptor and mints the NEXT monotonic sequence id - OCCURRENCE-identified, so an
  identical descriptor appended twice yields two DISTINCT ids (never content-addressed) -
  plus `DESC$`/`SEQ`/`EQUAL?`/`VALIDATE`/`COUNT` and the `ID>WIRE` (total) / `WIRE>ID`
  (`id-result`, fail-closed) 8-byte little-endian sequence codec. audit-event-id is the
  digest-EXCLUDED provenance link; raw conversions stay private. Owns -5342..-5345.
- `maki/journal-test.f` — occurrence identity (identical descriptor -> distinct ids,
  monotonic sequence), descriptor projection, the wire round-trip, fail-closed decode
  (wrong-width + unresolved sequence), cross-role rejection, and the private-mint
  unforgeability negatives.
- `maki/rev.f` — the revision identity registry + wire codec (the rev-id leg of
  MODEL-CAD-V2-PLAN.md § 23.9): `REV:COMMIT` content-addresses the canonical revision
  content (parent + write set, serialised by maki/db/transaction.f) to a
  `CAD-KIND:rev-id` (equal content shares one id, so deterministic replay reproduces the
  rev-id), plus `CONTENT$`/`EQUAL?`/`VALIDATE`/`COUNT` and two codecs: `ID>WIRE`/`WIRE>ID`
  (8-byte raw) and the cross-process `KEY>WIRE`/`WIRE>KEY` (32-byte SHA-256 content key
  over the interned revision content, § 23.9; the transaction revision chain becomes a
  Merkle chain of content keys). This leg owns rev-id IDENTITY; the revision-content
  canonical FORM lives in the transaction dot. Raw conversions stay private.
  Owns -5346..-5349.
- `maki/rev-test.f` — content-addressed COMMIT (equal content one id, distinct parent
  distinct id), the wire round-trip, fail-closed decode (wrong-width + unresolved raw),
  cross-role rejection, and the private-mint unforgeability negatives.
- `maki/sched-key.f` — section-7.4 schedule keys: the typed `skey` product (now
  carrying the region's requested `NPOL:dom` policy, REGION-POL's per-op OP-DOM fold,
  so a different policy is a different key), the durable `SK-KEY$` render (region
  signature, shape class,
  dtype/layout/align, numeric policy, facts-based target field, engine content key),
  and the replay-table seam.
- `maki/sched-key-test.f` — shape-class/dimclass identity, full-key pins, the
  same-label different-facts target negative, the per-op requested-policy key cases
  (a different op mix => a different honest policy => a different key), and
  replay-table regressions.
- `maki/competitive-report.f` — the checked BENCH competitive-comparison schema:
  four distinct nominal id families (workload/shape/protocol/baseline), the closed
  cache-state and absence sums, unit-typed present-or-absent throughput readings
  (`gbps`/`gflops` sums), and the two per-unit comparison products
  (`comparison-gbps`/`comparison-gflops`) so a GFLOP/s reading can never fill a
  GB/s slot - unit confusion is a value-level type mismatch. `GBPS-COMPARABLE?` /
  `GFLOPS-COMPARABLE?` are the pairing verdict over the two `NPOL:dom` numeric-policy
  witnesses (an exact-policy FP32 side and a relative-policy TF32 side are not
  comparable), and the canonical versioned `RENDER-GBPS`/`RENDER-GFLOPS` emit one
  byte-stable row per unit (every field alters the key). A generic `comparison<a>`
  is expressible but not used: a product type parameter binds only cell-tier types,
  so it would need TRUSTED nominal-cell unit witnesses and a unit-agnostic reading;
  the concrete per-unit families give stronger value-level unit typing with zero
  TRUSTED surface. Owns -5257..-5258.
- `maki/competitive-report-test.f` — the SAXPY FP32 and Habu-MMM/Triton TF32 byte
  goldens, per-field key-alteration cases (each id/policy/unit/value + cold/warm),
  the named-absence render, the numeric-policy pairing verdicts (the historical
  Habu-FP32-vs-Triton-TF32 confusion rejects with a resolving positive), the
  identity/raw-n/cache/unit/precision type-reject candidates with resolving
  positives, and the E-BENCH capacity throws.
- `maki/competitive-store.f` — the checked canonical codec + sealed store records for
  BENCH comparison values. The ONLY public writers (`BENCH-PUT-GBPS`/`-GFLOPS`) accept a
  typed comparison and DERIVE its exact key (the single `RENDER-GBPS`/`-GFLOPS` canonical
  render), the bench/v1 schema version, FNV-1a content digest, and promotion-evidence
  field (the `COMPARABLE?` verdict); the raw file appender `BENCH-ROW-APPEND` is
  package-BENCH-private (the store seal, mirroring the R7 EVID-PUT-G/SK-PUT-DURABLE seal),
  so nothing outside the package can plant a raw row. Rehydration (`BENCH-DECODE-GBPS`/
  `-GFLOPS`) treats persisted bytes as untrusted and fails closed per axis with a named
  throw: duplicate-field, malformed, bad label/token, wrong-schema, cross-kind,
  cross-policy, digest-mismatch, noncanonical (re-render mismatch), and stale-promotion.
  Reuses maki/store.f only through its PUBLIC root discipline (`STORE-ROOT+`/`STORE-RESET`,
  `competitive.rows`); the CAD store's own row schemas stay untouched. Owns -5310..-5320.
- `maki/competitive-store-test.f` — the gbps/gflops/absent byte goldens, the byte-for-byte
  persist -> rehydrate -> re-persist round-trips, one named-throw fixture per forgery class
  (each resolving against the clean base golden), and the store-seal bypass regression
  (`BENCH:BENCH-ROW-APPEND` unresolvable/verdict 1, paired read + typed-write controls).
- `maki/competitive-evidence.f` — the checked per-side competitive EVIDENCE row + the
  closed metric UNITS vocabulary (the § 22.10 matrix's richer schema over
  maki/competitive-report.f's throughput-only bench/v1). One measurement handle (package
  CEVID) carries full identity (workload/revision/shape/numeric-policy/target/compiler/
  cache-state/protocol/baseline over reused BENCH ids + CAD-KIND:target-id + NPOL:dom) and
  six typed metric readings (latency/throughput/bytes/launches/memory/energy), each a
  present-value-or-named-absence over the sealed `unit` enum
  {ns,ms,gflops,gbps,bytes,count,watts} (joules excluded: no measured corpus energy). A
  reading's unit must match its field CATEGORY (E-CEVID-UNIT); `COMPARABLE?`/`RENDER-PAIR`
  refuse a mismatched numeric DOMAIN (E-CEVID-INCOMPARABLE), so the Habu-FP32 vs Triton-TF32
  confusion can never form a comparison row; cache-state is a required field; `RENDER`/
  `RENDER-PAIR` emit byte-stable cevid/v1 / cevid-cmp/v1 rows. Rows are a bounded first-slice
  handle pool. Owns -5417..-5421.
- `maki/competitive-evidence-test.f` — the migrated flagship byte goldens (GEMM
  MMM-WIDE-B-M4-S1 3026.6 GFLOP/s = 1.60x Triton, SAXPY-V4 64.209 vs Triton 63.0 GB/s), the
  every-unit render, replay byte-stability, the cold/warm key-alteration, the numeric-policy
  pairing verdicts (the FP32-vs-TF32 reject + resolving positives), the E-CEVID unit-category
  / capacity / oversized-row throws, and the static numeric-policy/raw-n/identity-slot
  checker-reject candidates with resolving positives.
- `maki/competitive-evidence-store.f` — the DURABLE typed store codec for cevid/v1 evidence
  rows (reopens package CEVID; the persistence follow-on to the schema). ENCODE derives the
  KEY = the single canonical `RENDER` (every field participates, cache-state included) and
  wraps it in the schema=cevid/v1 versioned envelope; the store key is SHA-256(render) and
  the file is rows/hex(store-key), published crash-safe with `ATOMIC-WRITE-FILE` (temp+rename)
  and content-addressed (a re-PUT is byte-identical). DECODE treats persisted bytes as
  untrusted and returns the typed `load-result` sum (ok<evidence> / malformed), mapping the
  structural throws (schema/fields/label/token/canon + the schema's own E-CEVID-UNIT/-CAP) to
  `malformed` at the catch boundary and re-throwing any other code (IO/width/root never
  masquerade). LOAD adds absent (no file) and the content-path identity check (the file at
  this key must decode to the query render). No embedded content digest: the SHA-256 filename
  already commits to the content, so the canonical re-render is the integrity axis. Reuses
  maki/competitive-evidence.f's render only (no wire duplication); no run-metric MEASURE
  change (no runtime consumer needs per-metric units). Owns -5422..-5428.
- `maki/competitive-evidence-store-test.f` — the flagship GEMM/SAXPY encode goldens, the
  in-memory encode->decode->re-encode and durable PUT->fresh-LOAD->re-encode byte-for-byte
  round-trips, the typed absent/malformed LOAD verdicts (a planted foreign row + garbage), the
  content-key composition (cold vs warm is a distinct durable file), and one `malformed`
  forgery per DECODE axis (schema tag, meta schema, bad token, field count, bad label,
  wrong-category unit, over-capacity value, non-canonical spelling) each resolving against the
  clean base row.
- `tools/eval-triton.f` — migrates the SHIPPED SAXPY and GEMM competitive evidence
  (docs/eval-triton.md, tools/ptx/perf-rows.tsv) into the typed BENCH store/report path
  as an EXTERNAL consumer of the store seal (package EVAL-TRITON, public API only, no raw
  appender): the comparable results SAXPY-FP32 (exact/exact) and HABU-MMM-TF32 (rel/rel)
  persist through the sealed store, and the checked importers `IMPORT-GBPS-RESULT` /
  `IMPORT-GFLOPS-RESULT` refuse a non-comparable pair as a competitive result. The
  historical Habu-FP32-vs-Triton-TF32
  pair stays as separately-labelled source evidence that renders but never loads as a result.
  Owns -5321 (`E-BENCH-INCOMPARABLE`).
- `tools/eval-triton-test.f` — migration acceptance: byte-stable report goldens for the two
  results and the labelled historical pair; layer-level comparability verdicts; persist ->
  find-by-exact-key -> replay (get/decode/re-encode) byte-for-byte to the committed canonical
  rows; the lookup-invalidation case (cache warm->cold changes the key => GET misses); and the
  incomparable-import negative regression (`E-BENCH-INCOMPARABLE`) with a comparable-import
  resolving positive.
- `maki/cuda-types.f` — thin re-export of `lib/ptx/cuda-driver.f` preserving the
  historical maki spellings (cuda-* roles, `CUDA-HANDLE0`/`CUDA-RC0`, `E-MK-GPU`).
- `maki/cuda-types-test.f` — runtime regressions for CUDA handle and rc
  fail-closed helpers.
- `maki/cuda-driver.f` — thin re-export of `lib/ptx/cuda-driver.f` aliasing the
  legacy binding spellings (`CUDA:CUINIT` ... `CUDA:CUDEVICEPRIMARYCTXRELEASE`)
  over the hyphenated lib bindings so existing consumers keep working unchanged.
- `maki/cuda-driver-test.f` — portable CUDA Driver binding and fail-closed
  helper regressions.
- `maki/device-artifacts.f` — private per-grade artifact root, PTX/cubin path,
  cleanup, and `PTXAS` resolution helpers for device graders.
- `maki/device-artifacts-test.f` — focused coverage for grader artifact
  uniqueness and cleanup.
- `maki/eval/emit-device.f` — the device NUMERIC golden for the sumnorm/gemm/
  attention authoring tasks: emits each candidate, ptxas-assembles, runs on the
  Orin against a CPU reference, and takes max|err|. Owns the canonical
  wrong-but-green fixtures; keyed on the `EVND:ON-DEVICE?` device-FFI probe.
- `maki/eval/emit-device-test.f` — the Orin device-golden suite: correct kernels
  match the reference, sumnorm role/value swaps diverge numerically, gemm
  double-accumulate is ptxas-rejected, and the attention role swaps emit
  byte-identical PTX (a codegen no-op); a recorded SKIP off-device.
- `maki/eval/device-fault-test.f` — regression that a ptxas-clean-but-launch-
  faulting no-check candidate (raw span pointer used as the grid index -> OOB read
  -> contained GPU MMU fault) is GRADED as the `EVN-DEVICE-FAULT` bucket and the
  grader survives to grade the next candidate GREEN; a recorded SKIP off-device.
- `maki/async-dag.f` — typed immutable async execution DAG (V2 §22.3 host
  schema): nominal stream/event/node identities, the `akind` node sum
  (kernel/copy/memset/event-record/event-wait), explicit ordering edges, and
  the sealed deterministic topological replay order.
- `maki/async-dag-test.f` — identity-forging/role-swap checker negatives plus
  runtime coverage: replay determinism (byte-identical renders),
  use-before-ready, cross-stream missing wait, event double-destroy, cycle,
  sealed/unsealed misuse, wrong-kind payloads, stale handles, capacities.
- `maki/plan-ir.f` — plan-IR → execution handoff: lowers the adopted model IR
  into a sealed one-stream async DAG and replays the sealed order through the
  host executor (the typed replacement for hidden index-order sequencing).
- `maki/plan-ir-test.f` — handoff regressions: replay outputs match `EX-RUN`,
  order/kernel-payload pins, byte-identical rebuild renders, event no-op
  replay, and the empty/unsealed/unsupported fail-closed paths.
- `maki/typestate.f` — R7 stage typestate skeleton: one package per IR level
  (MODEL/TIR/RIR/PLAN/KIR/CAND/ART) with sealed arity-0 stage nominals and the
  transition words that thread the one legal pipeline order, so a wrong-stage
  input is a checker reject before runtime. ART:built is a PRODUCT carrying its
  `CAD-KIND:artifact-id` + a private build-proof token (identity threading), so
  downstream evidence/policy transitions read the artifact from the built witness.
- `maki/typestate-test.f` — stage-order acceptance: per-transition positive
  controls, wrong-stage negatives (unconstrained-model, draft-plan,
  unverified-KIR, reverse-stage), and a runnable MODEL→KIR pipeline.
- `maki/evidence/schema.f` — R7 artifact-indexed evidence families: one PRODUCT
  per class (certified/golden/gradchecked/profiled) binding a CAD-KIND:artifact-id
  and a class-private proof token, golden leg/precision/numeric-domain as fields
  (`GOLD-DOM` projects the achieved `NPOL:dom` for the promotion refusal gate),
  per-class presence slots + the promotion bundle, and the EVID:CERTIFY/GOLDEN/
  GRADCHECK/PROFILE mint transitions.
- `maki/evidence/schema-test.f` — evidence acceptance: born-typed construction
  positive controls, wrong-artifact-evidence negatives (raw token, foreign id,
  private-mint), wrong-class negatives, and bundle slot-order pins.
- `maki/evidence/policy.f` — R7 promotion-policy products: the `req` requirement
  ENUM (DERIVE eq), the `gate-set` promotion policy, the sealed `granted` grant
  (private grant-proof token), DEFAULT-POLICY (exactly the V1 gate set), the
  `POLICY:SCHEMA` schema-id producer (the V1 policy schema identity), and
  POLICY:CHECK — the one value-level artifact-binding site, reading the artifact
  under promotion from the ART:built witness (E-EVID-ARTIFACT / E-EVID-MISSING
  refusals over the EVID bundle/slot schema; artifact equality via ARTIFACT:EQUAL?).
- `maki/evidence/policy-test.f` — promotion acceptance: DEFAULT-POLICY /
  POLICY:CHECK / grant positive controls, missing-gate + forge negatives (raw
  token, foreign class, id-swap, private-mint, non-gate-set input), and executed
  white-box value cores (the SLOT-ERR decision table incl. the wrong-artifact
  E-EVID-ARTIFACT fact over real registered ids, the V1-gate-set default).
- `maki/evidence/policy-e2e-test.f` — executed end-to-end promotion: POLICY:CHECK
  run over a REAL EVID:bundle built from genuine values (ARTIFACT:REGISTER ids, a
  MODEL→…→ART:BUILD built witness, the real EVID gate transitions) — a matching
  bundle grants and binds the artifact, an absent required slot refuses
  E-EVID-MISSING, wrong-artifact evidence refuses E-EVID-ARTIFACT, and ART:PROMOTE
  refuses a grant issued for a different artifact (the tightening).
- `maki/evidence/promote.f` — R7 promotion transition + store seal: the sealed
  `ART:PROMOTE ( ART:built POLICY:granted -- ART:promoted )` (promotion needs the
  sealed grant, so a forged tag readout cannot reach `ART:promoted`) which now
  verifies the built witness and the grant name the SAME artifact
  (E-EVID-ARTIFACT on mismatch), the private `RAW>PROMOTED` stage mint. Companion
  to the maki/store.f store-row-writer seal and the maki/cad.f golden-provenance
  threading that retires the maki/golden.f ambient globals.
- `maki/evidence/promote-test.f` — promotion + store-seal acceptance: the
  ART:PROMOTE positive control, the missing-grant / private-mint negatives (forged
  promotion rejects through the checked path), and the store-bypass regression that
  the sealed `MAKI:EVID-PUT` / `MAKI:EVID-PUT-G` / `MAKI:SCHED-PUT` writers no longer
  resolve while the read surface still does.
- `maki/store-rehydrate.f` — R7 typed rehydration boundary for persisted store rows:
  `EVID-ROW-DECODE` parses an untrusted evidence-row suffix into its typed fields (four
  verdicts + `EVID:golden-leg` + `EVID:prec-class`) so a bad field count / unknown label
  / out-of-domain verdict / bad precision rejects with a named throw (never silent,
  never raw), plus the typed-render projections that go back through the sealed wire
  owner. No proof tokens minted on read; no new store-planting surface.
- `maki/store-rehydrate-test.f` — rehydrate acceptance: the typed-fields positive
  control, one negative per failure class (`E-EVID-ROW-*`: bad field count, unknown
  label, bad verdict, bad precision), the device+host round-trip goldens
  (persist→rehydrate→re-persist byte-identical), and the `SK-PUT-DURABLE` store-seal
  regression (sealed writer verdict-1, paired read control).
- `maki/maki.f` — Maki one-file entry point: host-framework `require` aggregator
  plus the curated top-level `MAKI:` surface, re-exporting the model-authoring /
  train / eval workflow words (losses, optimizers, ONNX import, checker-as-judge
  core) from their subsystem packages via `EXPORT`. Load it to call `MAKI:WORD`
  or drill into `LOSS:`/`OPTIM:`/`ONNX:`/`EVAL:`/`PLAN:`/`REPORT:` directly.
- `maki/test.f` — Maki-owned checked test-suite entry point; lists maki test
  files only and reports per-test pass/fail timing outside the Habu trust root.
- `tools/srclist.f` — canonical source order.
- `tools/stdin-closure-lib.f` — canonical stdin driver closure manifest (single source of truth for gate 17e).
- `tools/stdin-closure-lint.f` — fail-closed drift gate proving stdin-closure consumers stay reconciled with the manifest.
- `tools/build-fixpoint.f` — checked self-rebuild fixpoint orchestration definitions.
- `tools/build-fixpoint-main.f` — CLI entrypoint for the self-rebuild fixpoint driver.
- `tools/build-fixpoint-test.f` — checked fixture coverage for the self-rebuild fixpoint driver, including warm-snapshot execution of the persisted compile-immediate preflight hook.
- `tools/boot-pin.f` — boot-prefix content-pin tool: print/verify the digest of the checker/core source the engine re-reads at boot.
- `tools/boot-pin-main.f` — CLI entrypoint for the boot-prefix pin tool.
- `tools/lint/json-writer.f` — compact JSON writer for native lint diagnostics.
- `tools/lint/source-lex.f` — checked vector-backed source lexer for native lints.
- `tools/lint/text.f` / `tools/lint/token.f` / `tools/lint/intern.f` /
  `tools/lint/lib.f` — shared native lint foundation: checked text/file
  helpers, the whitespace token table, the growable string interner, and the
  scanner core.
- `tools/lint/text-foundation-test.f` / `tools/lint/set-test.f` — focused
  coverage for the lint text helpers and interner.
- `tools/lint/shadow-lint.f` / `tools/lint/shadow-lint-test.f` — rejects GLOBAL-scope
  toolchain definitions that shadow engine PRIM names; string-literal bodies and
  package-scoped tails are ignored. Its `tools/lint/shadow-string-fixture.f`
  string-literal regression input is a committed filemap-lint exclusion.
- `tools/lint/clobber-lint.f` / `tools/lint/clobber-lint-test.f` —
  register-clobber analysis for BL-able emitter routines and its regressions,
  including wrapped emitter calls (a `PKG:CALL` macro that emits a
  branch-with-link, e.g. `PROT-GUARD:CALL`); its negative syscall-scratch and
  wrapped-call fixture files are committed filemap-lint exclusions.
- `tools/signature-lint-core.f` — reusable strict typed-signature lint core.
- `tools/signature-lint.f` — CLI wrapper for strict typed-signature lint.
- `tools/signature-lint-test-lib.f` — load-only strict typed-signature lint fixture library for resident runner tests.
- `tools/signature-lint-test.f` — checked fixture coverage for strict typed-signature lint.
- `tools/reserved-name-lint-core.f` — reusable source scanner rejecting definitions named like parser/control reserved words (`I`, `J`, `LOOP`, `TRUST`, etc.).
- `tools/reserved-name-lint.f` — CLI wrapper for reserved definition-name lint; run after generated prefix-stripping/naturalization.
- `tools/reserved-name-lint-test-lib.f` — load-only reserved-name lint fixture library for resident runner tests.
- `tools/reserved-name-lint-test.f` — checked fixture coverage for reserved definition-name lint and `tools/check.f` preflight behavior.
- `tools/duplicate-definition-lint-core.f` — reusable source scanner rejecting duplicate published definitions across source lists.
- `tools/duplicate-definition-lint.f` — CLI wrapper for duplicate published-definition lint.
- `tools/duplicate-definition-lint-test-lib.f` — load-only duplicate-definition lint fixture library for resident runner tests.
- `tools/duplicate-definition-lint-test.f` — checked fixture coverage for duplicate definition detection and source-list preflight behavior.
- `tools/typed-local-diff-lint-core.f` — reusable diff scanner that rejects newly added bare locals.
- `tools/typed-local-diff-lint.f` — CLI wrapper for typed-local diff lint.
- `tools/typed-local-diff-lint-test.f` — checked fixture coverage for typed-local diff lint.
- `tools/kernel-perf-lint-core.f` — reusable diff scanner requiring kernel codegen changes to carry a profile/waiver row in `tools/ptx/perf-rows.tsv`.
- `tools/kernel-perf-lint.f` — CLI wrapper for the kernel profile-row diff lint.
- `tools/kernel-perf-lint-test.f` — checked fixture coverage for the kernel profile-row diff lint.
- `tools/aot-lint-core.f` — reusable stripped-AOT unsupported-word scanner.
- `tools/aot-lint.f` — CLI wrapper for stripped-AOT unsupported-word lint.
- `tools/aot-lint-test-lib.f` — load-only stripped-AOT lint fixture library for resident runner tests.
- `tools/aot-lint-test.f` — checked fixture coverage for stripped-AOT source lint.
- `tools/diag-origin-core.f` — reusable source-origin marker injection core.
- `tools/diag-origin.f` — CLI wrapper for checker JSON source-origin markers.
- `tools/diag-origin-test-lib.f` — load-only diagnostic-origin fixture library for resident runner tests.
- `tools/diag-origin-test.f` — checked fixture coverage for diagnostic origin markers.
- `tools/json-only-core.f` — reusable JSON diagnostic line filter core.
- `tools/json-only.f` — keeps wrapper JSON mode machine-only on known diagnostics.
- `tools/json-only-test-lib.f` — load-only JSON-only fixture library for resident runner tests.
- `tools/json-only-test.f` — checked fixture coverage for JSON diagnostic filtering.
- `tools/json-file.f` — dynamic file-backed JSONL cursor shared by benchmark validators and reducers.
- `tools/gate-json-assert-core.f` — native JSON assertion library for gate tests.
- `tools/gate-json-assert.f` — CLI entrypoint for native JSON assertions.
- `tools/repair-schema-doc-test.f` — checked fixture coverage for repair diagnostic schema docs.
- `tools/repair-packet-core.f` — reusable checker JSONL to repair-packet core.
- `tools/repair-packet-test.f` — checked fixture coverage for repair packet generation.
- `tools/diagnose-hb-core.f` — checks bin/hb's baked source-prefix set against a root; names the first unresolved file behind the opaque exit-74 outside the repo.
- `tools/diagnose-hb-test.f` — checked fixtures for the hb-outside-repo prefix diagnostic.
- `tools/hb-open-failure-test.f` — regression: the built engine names the first unresolved baked prefix source on stderr and exits 74 when started outside the repo.
- `tools/check-repair-hints-test.f` — checked fixture coverage for repair-class hints.
- `tools/host-lint.f` — rejects retired host-script workflow tokens.
- `tools/process-primitive-lint-core.f` — confines raw spawn/fork primitives to
  the checked process wrapper modules and checker primitive declarations.
- `tools/process-primitive-lint.f` — live-tree raw process primitive lint.
- `tools/process-primitive-lint-test.f` — comment/string, raw-call, and owner
  allowlist coverage for process primitive confinement.
- `tools/bootstrap-mirror-lint.f` — tripwire: ADT declarations must not enter the gforth-compiled recovery corpus (src/) before the stage-0 pass-2 mirror lands.
- `tools/bootstrap-mirror-lint-test.f` — focused coverage for the recovery-corpus tripwire (clean src walk + planted overlay).
- `tools/check-all-errors-core.f` — reusable all-errors checker core; keeps
  per-definition checker runs as the diagnostic isolation boundary.
- `tools/check-all-errors.f` — CLI wrapper for all-errors checking.
- `tools/check-all-errors-test.f` — checked fixture coverage for all-errors checking.
- `tools/checked-boundary-lint-core.f` — reusable unchecked-boundary scanner core for checker and CLI paths; requires canonical compile-preflight rearm after `0 set-check`.
- `tools/checked-boundary-lint.f` — CLI wrapper for unchecked-boundary lint.
- `tools/checked-boundary-lint-test-lib.f` — load-only unchecked-boundary lint fixture library for resident runner tests.
- `tools/checked-boundary-lint-test.f` — checked fixture coverage for unchecked-boundary lint.
- `tools/diag-to-sarif-core.f` — reusable diagnostic JSONL to SARIF converter core.
- `tools/diag-to-sarif.f` — CLI entry for diagnostic JSONL to SARIF conversion.
- `tools/public-signatures-core.f` — reusable public-signature manifest emitter core.
- `tools/public-signatures.f` — CLI entrypoint for typed public-word manifests.
- `tools/public-signatures-test.f` — checked fixture coverage for public-signature manifests.
- `tools/stdlib-manifest-test.f` — validates `lib/std.manifest`, stdlib docs, and source-backed signatures.
- `tools/spawn-emitter-test.f` — source-shape coverage for factored Darwin
  spawn primitive emitters in `src/habu/habu1.f`.
- `tools/c-call-emitter-test.f` — source-shape coverage for factored native
  `C-CALL` inline/call emitter phases in `src/habu/habu2.f`.
- `tools/signature-scan-emitter-test.f` — source-shape coverage for shared
  signature scanner/capture emitters in native and recovery codegen.
- `tools/compiler-dispatch-test.f` — source-shape coverage for factored compiler
  dispatch and code-section emitter groups in native and recovery codegen.
- `tools/codegen-role.f` — structural codegen-role checks: extracts guarded
  emitter definitions from the real stage sources, certifies and runs them on
  the live arm64 primitives, and asserts branch-fixup and store-slot roles.
- `tools/codegen-role-test.f` — positive coverage plus corruption fixtures for
  the codegen-role checks, replacing the retired BF-PREFLIGHT textual asserts.
- `tools/aot-call-report-lib.f` — reusable AOT call-stencil report scanner.
- `tools/aot-call-report.f` — CLI entrypoint for AOT call-stencil reports.
- `tools/aot-call-report-test.f` — checked fixture coverage for AOT call-stencil reports.
- `tools/bundle-lib-core.f` — reusable stdlib bundle construction core.
- `tools/bundle-lib.f` — CLI wrapper for stdlib bundle construction.
- `tools/bundle-lib-test-lib.f` — load-only stdlib bundle fixture library for resident runner tests.
- `tools/bundle-lib-test.f` — checked fixture coverage for the stdlib bundle tool.
- `tools/examples-test.f` — checked fixture coverage for stdlib examples.
- `tools/filemap-lint.f` — freshness and completeness lint for this file:
  listed paths must exist, and every .f/.fs file under the src, tools, test,
  and lib roots must be listed unless a committed exclusion row names it.
- `tools/filemap-lint-test.f` — fixture coverage for the derived filemap
  policy: unlisted policy files, stale listed paths, stale exclusions, and
  missing required docs all fail closed.
- `tools/repl-lint-core.f` — reusable scanner rejecting REPL-baked code that exits the interactive session.
- `tools/repl-lint.f` — CLI wrapper for REPL exit lint.
- `tools/repl-lint-test-lib.f` — load-only REPL exit lint fixture library for resident runner tests.
- `tools/repl-lint-test.f` — checked fixture coverage for REPL exit lint.
- `tools/trust-lint-core.f` — reusable `TRUSTED.md` drift scanner core.
- `tools/trust-lint.f` — CLI wrapper for `TRUSTED.md` drift lint.
- `tools/trust-lint-test.f` — checked fixture coverage for `TRUSTED.md` drift lint.
- `tools/trusted-inventory.f` — TRUSTED ratchet: lexer-backed TSV inventory of `TRUSTED:`/`TRUST`/`0 set-check` sites plus baseline compare against `TRUSTED.md`.
- `tools/trusted-inventory-test.f` — checked fixture coverage for the trusted-inventory ratchet, plus a live child run of the production `strict` trust-gate command.
- `tools/primitive-effect-inventory.f` — PEINV ratchet: independent identity inventory of the `PRIM:`/`PPRIM:` axiom rows (canonical kind/package/spelling/effect/flags tuple), cross-checked against the live `#PE` registry and baselined against the `primitive-effect-inventory-manifest` block in `TRUSTED.md`.
- `tools/primitive-effect-inventory-test.f` — checked fixture coverage for the primitive-effect inventory ratchet: identity round-trip, formatting stability, and the add/delete/duplicate/reorder/mutation tamper matrix.
- `tools/refine-lint-core.f` — inventory-driven confinement lint for TRUSTED refinement mints (rows shaped `n -- <nominal family>`): references outside the owning file are findings unless cited by the row's Tests cell or an explicit allowlist; a seed list cross-checked against `TRUSTED.md` plus a mint-shape scan keeps the set from rotting. Interim until the TVK-RAW checker capability lands.
- `tools/refine-lint.f` — CLI wrapper for the refinement-mint confinement lint.
- `tools/refine-lint-test.f` — checked fixture coverage: shape/policy fixtures, a red scratch-file mint call outside the tree, and the green live tree run.
- `tools/suite-coverage-lint-core.f` — derives the gate suite lists from test/gate-stdlib-cases.f, test/gate-stdlib-inline-lib.f, test/gate-stdlib-lint-tools.f, test/run-worker-stdlib.f, test/gate-engine-lib.f, and test/candidate-validation.f each run; enforces that every TEST:SUITE member is scheduled/manual-documented/spawn-only-documented and that the inprocess GSI-LINT-LIBS-PTX-TOOL list equals the spawned ptx-toolchain list minus the documented spawn-only bench set.
- `tools/suite-coverage-lint.f` — CLI wrapper for the stdlib gate suite-coverage lint.
- `tools/suite-coverage-lint-test.f` — checked fixture coverage for the suite-coverage lint: BOL member parsing, orphan detection, ptx missing/extra/spawn-only divergence, exact-equality clean case, and manual/spawn-only table staleness.
- `tools/host-lint-test.f` — focused coverage for host-script lint policy helpers.
- `tools/stale-status-lint-core.f` — reusable stale status/count lint core.
- `tools/stale-status-lint.f` — CLI wrapper for stale status/count lint.
- `tools/stale-status-lint-test.f` — checked fixture coverage for stale status/count lint.
- `tools/parallel-agent-lint.f` — freshness lint for the parallel-agent protocol.
- `tools/dot-dep-lint-core.f` — reusable dot blocker dependency validator.
- `tools/dot-dep-lint.f` — CLI wrapper for dot blocker dependency lint.
- `tools/dot-dep-lint-test.f` — checked fixture coverage for dot blocker dependency lint.
- `tools/maki-dep-lint-core.f` — one-way habu<-maki dependency guard: token-scans src/ lib/ test/ for a forbidden maki/ path reference.
- `tools/maki-dep-lint.f` — CLI wrapper for the maki one-way dependency lint.
- `tools/maki-dep-lint-test.f` — checked fixture coverage for the maki one-way dependency lint.
- `tools/namespace-lint-core.f` — flags maki definitions at global scope (outside any subsystem package): case-insensitive definer/package matching (the dictionary is case-insensitive) including `KERNEL:`, whitelisting E-* constants, the documented ARRAY substrate, and legacy BEGIN-/END- pairs; the active maki-namespace guard (subsumed the retired maki-ns-lint, dot habu-maki-ns-lint-reconcile).
- `tools/namespace-lint.f` — CLI wrapper for the maki namespace lint (enforcing: throws on any global-def finding).
- `tools/namespace-lint-test.f` — checked fixture coverage for the maki namespace lint (detection, case-insensitivity, scope, whitelist, live strict sweep).
- `tools/error-code-lint-core.f` — global E- throw-code uniqueness lint: flags a negative code claimed by two different E- names across src/ lib/ tools/ test/ maki/.
- `tools/error-code-lint.f` — CLI wrapper for the E- throw-code uniqueness lint (enforcing).
- `tools/error-code-lint-test.f` — checked fixture coverage for the E- throw-code uniqueness lint.
- `lib/string-test.f` — focused coverage for checked string helpers.
- `lib/json-write.f` — checked emit-only JSON writer vocabulary for fixtures and native tools.
- `lib/json-write-test.f` — focused coverage for JSON writer escaping, structure, and errors.
- `lib/json-read.f` — checked zero-allocation JSON pull/cursor parser complementing the writer.
- `lib/json-read-test.f` — focused coverage for JSON parser tokens, escapes, structure, errors, and round-trip.
- `lib/memory.f` — checked OS-backed byte buffer allocation helpers.
- `lib/memory-test.f` — focused coverage for memory allocation and 64K buffer spans.
- `lib/type/deftype.f` — the `DEFTYPE` declaration surface for value-nominal integer types: mints a package-scoped arity-0 type-family nominal plus its generated `>NAME`/`NAME>N` converter pair (TFAM substrate; docs/value-nominal-substrate.md).
- `lib/vector.f` — checked growable cell-vector helpers backed by OS memory.
- `lib/vector-test.f` — focused coverage for vector growth, bounds, typed pointer storage, and iteration.
- `lib/layout/box.f` — boxed-layout record arena (TFAM 16): bump-allocated tag+payload heap records over the mmap allocator, arena free-all ownership.
- `lib/layout/box-test.f` — focused coverage for box record alloc, tag/payload round-trip, chunk growth, and arena reset.
- `lib/adt/option.f` — the shared `option<T>` sum family (some value / none), the checked replacement for -1/sentinel returns (switchover wave A); require before consumers.
- `lib/adt/result.f` — the shared `result<ok,err>` sum family (ok value / err value), the checked replacement for value+flag/rc-plus-value returns where the flag distinguishes DIFFERENT errors (switchover wave B); require before consumers.
- `lib/adt/result-test.f` — focused proof that result<ok,err> constructs (RESULT:OK/ERR), MATCHes both arms, and rejects swapped ok/err payload types.
- `lib/cad-num-types.f` — package CAD-NUM scalar nominal numeric roles (byte-len, item-count, cell-count, index, byte-off, cell-off, alignment, positive-divisor, alloc-byte-len, alloc-cell-count) plus the on-stack `numeric-result<a>` sum; public checked validators wrap private audited `TRUSTED:` mints (MODEL-CAD-V2-PLAN.md B5.1). UNSEALED: no production entry loads it; sealing/arithmetic are separate dots.
- `lib/cad-num-types-test.f` — CAD-NUM B5.1 boundary matrix (every role x valid/zero/negative/overflow) and the static cross-role/raw-n rejection candidates; the sole consumer of lib/cad-num-types.f.
- `lib/nominal/arena.f` — package NOM growable index-addressed cell arena: copy-on-grow storage with stable indices and a seal high-water that makes committed spans immutable (E-NOM-SEALED); the storage substrate under path/binding/row records.
- `lib/nominal/path.f` — package NOM persistent consed lexical paths of opaque atom cells (no fixed depth), the opaque nominal `path` handle over node indices, root-first canonical order, and the scope-prefix REMAP-PATH; private audited MINT-PATH/PATH-IDX boundary.
- `lib/nominal/binding.f` — package NOM binding pool `[path,slot]` records, the opaque nominal `binding`, sorted chunks, and CHUNK-MERGE (the sorted-chunk stream behind UNION with idempotent dup collapse and E-NOM-CONFLICT); private MINT-BINDING/BIND-IDX.
- `lib/nominal/codec.f` — package NOM canonical content codec: streaming big-endian digest, wire encode, and validating decode (bounds, sortedness, path uniqueness, full consumption -> E-NOM-WIRE); identity is the canonical digest, never a handle number.
- `lib/nominal/row.f` — package NOM immutable row arena: digest-interned sorted-unique binding sets, PUBLISH-CHUNK (the shared FREEZE/UNION/REMAP publication path), content EQUAL?/KEY, and UNION/REMAP; private MINT-ROW/ROW-IDX.
- `lib/nominal/builder.f` — package NOM linear transactional builder: checker-enforced noncopyable `nom-builder` token, NEW/ADD/FREEZE/ROLLBACK with sort-once freeze and auto-rollback, and the per-transaction resource budget; private MK-BUILDER/BUILDER-DROP.
- `lib/nominal/snapshot.f` — package NOM snapshot/AOT adapter: single-row ENCODE/DECODE plus SNAPSHOT/RESTORE of all rows in canonical digest order (allocation-order-independent fixpoint bytes) with header/magic validation; NOM:RESET re-mmaps process-local arena pointers.
- `lib/nominal/nominal-test.f` — focused boundary + failure-matrix + static-rejection suite for the nominal collection (forgery, protected spans, linear-dup rejection); direct-loaded gate home.
- `lib/nominal/nominal-prop-test.f` — metamorphic property suite: order independence, per-binding dedup idempotence, union commutativity, and codec round-trip over pseudo-random binding sets.
- `lib/nominal/nominal-scale-test.f` — scale suite at 4096 distinct bindings (compose/remap/union/snapshot/replay) with the measured non-quadratic build-time bound.
- `src/cad/effect-types.f` — package CAD-EFFECT finite semantic-effect vocabulary (MODEL-CAD-V2-PLAN.md R8): the `effect-atom` sum (pure + 9 effectful atoms), the `slot-kind` sum (operand/attribute/capability/capture), and the four conservative truth tables `DUP-OK?`/`CACHEABLE?`/`BARRIER?`/`COMMUTE?` over atoms. No trusted mints. UNSEALED: only src/cad/effect.f and the focused suites consume it.
- `src/cad/effect.f` — package CAD-EFFECT canonical effect-row algebra over lib/nominal (dot habu-define-finite-cad-0bdf52ad): the public `effect-row` brand for a NOM row, transactional `NEW`/`EMIT`/`FREEZE`/`ROLLBACK`, `PURE`, `UNION`, `REMAP`, canonical `EQUAL?`/`SIZE`/`KEY`/`ENCODE`/`DECODE`/`SNAPSHOT`/`RESTORE`, and the wire-mask row classifiers `VALIDATE`/`ROW-BARRIER?`/`ROW-DUP-OK?`/`ROW-CACHEABLE?`/`ROWS-COMMUTE?`. Two audited `NOM:ROW>EFF`/`NOM:EFF>ROW` identity casts bridge NOM's package-private `row` type into `effect-row`.
- `src/cad/effect-test.f` — focused boundary, truth-table, rejection-matrix (stale/foreign/malformed/protocol-count/budget/duplicate/pure-atom/negative-index/negative-site/double-open), REMAP/UNION/canonical, and static-rejection suite for the CAD effect vocabulary + row algebra; direct-loaded gate home.
- `src/cad/effect-prop-test.f` — metamorphic property suite over pseudo-random effect-binding sets: order independence, UNION idempotence/commutativity, REMAP determinism, and canonical codec round-trip decided by content.
- `src/cad/effect-scale-test.f` — scale suite at 4096 distinct effect bindings (compose/remap/union-to-8192/classify/snapshot/replay) with the measured non-quadratic build-time bound.
- `lib/cad-num-arithmetic.f` — package CAD-NUM (reopened) B5.2 closed role arithmetic: extent add/sub, offset/index advance/retreat/distance, count multiply/scale, total positive-divisor div/rem, cell<->byte conversions, align-up, and alignment predicates. Overflow/underflow/misalignment are returned `numeric-result` variants; success values route through slice 1's public validators (no minting here); six private `TRUSTED:` role->n projections complete the read set. UNSEALED (MODEL-CAD-V2-PLAN.md B5.2).
- `lib/cad-num-arithmetic-test.f` — CAD-NUM B5.2 arithmetic matrix: every table row's zero/safe-max/first-overflow/underflow/misalignment case (value-checked on ok, variant-checked on failure) plus the static correct-signature/cross-role/reversed-operand candidates; the sole consumer of lib/cad-num-arithmetic.f.
- `lib/ffi-abi.f` — package-scoped AAPCS64 staging without generic checked call
  authority; trusted-only bounded trampolines carry distinct x0-x8 and stack
  writable-extent tables, plus float, kernelParams, and return support.
- `lib/ffi-abi-test.f` — fixed-schema writer and exact mixed-ABI coverage,
  including positive x8/stack pointers and zero-extent/schema-lie rejects.
- `lib/ffi.f` — compatibility entry for the sealed `FFI` package; the exact
  package-scoped dlopen/dlsym bindings keep legacy global aliases without any
  binding generator or mutable function-pointer export.
- `lib/ffi-test.f` — sealed scalar/read-only binding, dynamic-loader, nominal
  role, and trusted-only raw-capability coverage.
- `lib/task.f` — checked pthread-backed tasking: task TCBs, per-task region
  re-entry, task-local user variables, halt/join teardown, and mutex facilities.
- `lib/task-test.f` — focused coverage for two task workers, atomic shared
  progress, mutex-protected increments, and cooperative halt/join cleanup.
- `lib/float.f` — checked decimal string to IEEE-double parsing (STR>FLOAT) with power-of-ten scaling.
- `lib/float-test.f` — focused coverage for STR>FLOAT sign, fraction, exponent, and rejection cases.
- `lib/fmath.f` — shared transcendental exp core (FEXP) by in-Habu range reduction; used by maki/fmath.f and lib/ptx/ad-dag-eval.f.
- `lib/fmath-test.f` — known-value FEXP coverage across the range-reduction domain + central-difference check.
- `lib/fmt.f` — checked number formatting into the string builder: unsigned/signed ints and fixed-decimal floats.
- `lib/fmt-test.f` — focused coverage for integer and fixed-decimal float formatting and rounding.
- `lib/sort.f` — in-place ascending heapsort of float cell arrays (O(n log n), no scratch buffer).
- `lib/sort-test.f` — focused coverage for FSORT! ordering, duplicates, negatives, and degenerate lengths.
- `lib/stats.f` — float-array summary statistics: sum/mean/min/max/variance/stddev/percentile/median.
- `lib/stats-test.f` — focused coverage for the stats reductions and interpolated percentiles.
- `lib/hashmap.f` — open-addressing integer-key hash probe over caller-supplied arrays (O(1) amortized lookups).
- `lib/hashmap-test.f` — coverage for HASH64/HM-PROBE/HM-CLEAR including collision probing and clear.
- `lib/prelude.f` — checked boolean/flag (true/false/0<>) and float-stack (fdrop/fdup/fover/f<=/f>=) conveniences core omits.
- `lib/prelude-test.f` — coverage for the prelude flag and float-stack helpers.
- `lib/render.f` — byte buffer + CSV/JSON/Markdown formatters (RB-MILLI3/FIXED3/RATIO4/FFIX3) and a key/value DSL (MD-*/CV*/KV*).
- `lib/render-test.f` — coverage for the render formatters and key/value DSL.
- `lib/report.f` — declarative table reporting engine: declare columns once, render to CSV or Markdown.
- `lib/report-test.f` — coverage for the report engine (one column set to CSV + Markdown).
- `lib/test/runner.f` — checked test runner foundation for temp roots, captures, and aggregate failures.
- `lib/test/runner-test.f` — focused coverage for test runner process, timeout, and failure aggregation helpers.
- `lib/test/outcome.f` — checked assertions over the process outcome sum for capture-consuming tests.
- `lib/test/outcome-test.f` — focused coverage for the outcome assert helpers.
- `tools/date.f` — shared checked UTC Gregorian date parsing, formatting, and timestamp helpers.
- `tools/date-test.f` — focused coverage for shared date helpers.
- `tools/size-report.f` — parse an engine size map (HABU_ENGINE_SIZE_MAP build output) and render the exact per-contributor byte attribution: emitter-phase rows, the post-sign container rows (header, text-pad, target tail), code/text/container subtotals, engine file size, distance-to-page-floor, and a fail-closed reconciliation that itemises any unattributed residue as its own row.
- `tools/size-report-main.f` — CLI entry for the size-attribution report; prints the attribution then reconciles to a nonzero exit on any residue.
- `tools/size-report-test.f` — focused coverage for the size-report map parser and reconciler (last-block wins, container vs code partition, code/text/container subtotals, distance-to-floor).
- `lib/process-env.f` — checked child envp builder and PATH lookup helpers.
- `lib/process-env-test.f` — focused coverage for child envp and executable lookup.
- `lib/process-fork.f` — checked fork wrappers for resident copy-on-write workers.
- `lib/process-cwd.f` — checked child cwd process helpers layered on prepared argv/envp.
- `lib/process-cwd-test.f` — focused coverage for child cwd spawn, capture, cleanup, and validation.
- `lib/source.f` — checked source materialization and source-list transforms.
- `lib/source-test.f` — focused coverage for source materialization helpers.
- `test/process-env-child.f` — child fixture used by process-env tests.
- `tools/argv.f` / `tools/argv-test.f` — checked argv parser for tool
  script arguments and its coverage.
- `tools/json.f` / `tools/json-test.f` / `tools/json-file-test.f` — bounded
  JSON/JSONL parser and compact writer plus parser and file-cursor coverage.
- `tools/repair-packet.f` — CLI entrypoint for repair packet generation.
- `tools/diagnose-hb.f` — CLI entry that reports why bin/hb exits 74 outside the repo.
- `tools/bench.f` — fixed Habu-timed kernels run on `bin/hb`.
- `tools/ddc-verify.f` / `tools/ddc-verify-test.f` / `tools/ddc-drive.f` — Diverse Double-Compiling audit: builds bin/hb via the native fixpoint and the Gforth recovery chain and requires byte-identical output; explicit `HABU_ALLOW_BOOTSTRAP=1` audit, not per-commit.
- `tools/ddc-scheduled.f` / `tools/ddc-scheduled-drive.f` / `tools/ddc-scheduled-test.f` — change-triggered DDC gate: a content-key over `src/habu`/`src/arch`/`bootstrap/cg` vs the committed marker `tools/ddc-marker.txt`; runs the DDC audit only when the bootstrap chain changed and fails loudly on divergence.
- `tools/why-threw.f` — throw-site diagnostic: runs a quotation under `catch` and, on a nonzero throw, reports the code plus the live fill of the shared string builders (SB, content-key CK/CK-ROW) before re-throwing, so an opaque capacity code (e.g. E-STR-CAPACITY) names its buffer in fork-worker/parallel-gate captures.
- `tools/xref-test.f` — focused coverage for live dictionary xref words.
- `tools/asm-src-test.f` / `tools/asm-checked-test.f` — ARM64 encoder source
  regression and the checked encoder layout regression.
- `tools/image-bytes-test.f` — shared executable image byte writer regression.
- `tools/stdlib-errors-test.f` / `tools/stdlib-date-test.f` /
  `tools/stdlib-time-test.f` — focused stdlib coverage for `lib/errors.f`,
  `lib/date.f`, and `lib/time.f`.

## Stdlib Modules

- `lib/errors.f` — canonical stdlib throw codes.
- `lib/date.f` / `lib/time.f` — checked Gregorian UTC date helpers and native
  clock wrappers.
- `lib/argv.f` / `lib/argv-test.f` — checked argv parser for
  script arguments under `bin/hb` and its coverage.
- `lib/array.f` / `lib/array-test.f` — checked cell-array helpers and their
  coverage.
- `lib/build.f` / `lib/build-test.f` — checked helpers for Habu build scripts
  and their coverage.
- `lib/build-cache.f` / `lib/build-cache-test.f` — canonical persistent
  build-cache resolution, retained failure evidence, typed source
  classification, and report fixtures.
- `lib/codegen.f` / `lib/codegen-test.f` — shared bounded byte buffer for
  building generated Forth source (reset / append-byte / append-string /
  append-decimal / contents), used by the maki and deftype definers.
- `lib/codesign.f` / `lib/codesign-test.f` — checked executable promotion and
  ad-hoc signing helpers and their coverage.
- `lib/fs.f` / `lib/fs-test.f` — checked filesystem helpers (walks, reads,
  stat) and their coverage.
- `lib/fs-root.f` — checked write-and-search directory predicate for persistent
  roots.
- `lib/fs-mutate.f` / `lib/fs-mutate-test.f` — checked filesystem mutation
  helpers (mkdir, remove, rename, cleanup) and their coverage.
- `lib/map.f` / `lib/map-test.f` — fixed-capacity open-addressed string-key
  map layout and its coverage.
- `lib/process.f` / `lib/process-test.f` — checked process helpers, successful
  exec/fork observation hooks, and their coverage.
- `lib/process-argv.f` / `lib/process-argv-test.f` — checked argv process
  helpers and their coverage.
- `lib/process-command.f` / `lib/process-command-test.f` — checked
  command-owned process runner and its coverage.
- `lib/property.f` / `lib/property-test.f` — checked property-based test
  helpers and their coverage.
- `lib/regex.f` / `lib/regex-test.f` — bounded capture-free regex
  scanner/tokenizer and its coverage.
- `lib/table.f` / `lib/table-test.f` — checked fixed-capacity cell table
  helpers and their coverage.

## Tests And Benchmarks

- `test/checker-assert.f` — shared quiet checker-candidate assertion helper for
  negative checked-source tests.
- `test/drec-shape-test.f` — checked-prim surface pins for the typed
  dictionary-record capability: the record access shapes the XREF-*/BFR-*/
  BP-SLOT-* rewrite relies on compile checked today, and the two PES gaps
  (`dbase@` provenance, `patch32` ptr overload) stay rejected until the
  engine lane closes them deliberately.
- `test/nf.fs` — Gforth-hosted native-Forth build/run/capture harness used by
  the no-binary bootstrap path.
- `test/bootstrap-wide-memory.fs` — Gforth-hosted execution gate that builds a
  stage0 compiler and requires the wide-memory subject to print `ok`.
- `test/bootstrap-wide-memory-src.f` — checked W2/W4 ADT store/fetch subject;
  pins stage0 instruction goldens, canonical cell order, mixed/branch locals,
  and runtime round trips. Runs under the Gforth recovery gate AND natively in
  the engine validate slice (gate-engine-lib GE-WIDE-MEMORY-SUITE), so native
  codegen drift from the recovery path fails in the ordinary gate.
- `test/bootstrap-wide-interpret-src.f` / `test/bootstrap-wide-tick-src.f` —
  stage0 negative execution fixtures proving published wide-effect words cannot
  run or be ticked from interpretation state.
- `test/atomics-smoke.f` / `test/run-in-stack-smoke.f` — tasking primitive
  smoke tests for atomics and the in-stack runner.
- `test/internal-word-gate.f` — engine-internal execution-gate regressions, including sealed field mutation and checked read-only field reflection.
  (dot habu-hb-crash-bare-c5be6634): bare/ticked internal checker colon words
  fail closed with `hb: internal engine word:` + rc 70 on both cold-prefix
  paths; positives pin E-UNDEFINED/E-UNDERFLOW, unchecked user words, TRUST/
  TRUSTED:, the structures + type-family DSLs, and XREF introspection.
- `test/immediate-model-test.f` — p5 wrong-certificate regressions (dot
  habu-checker-fitting-arity-70dc94e4): a signature-carrying live immediate in
  a checked body rejects (fitting-arity and no-op shapes), pinning that the
  checker never certifies a declared effect the empty runtime body cannot
  deliver; declared parsing immediates (`parse-imm`) stay green on both the
  raw-text and engine-hook scan paths.
- `test/underdepth-gate.f` — certified-word interpret underdepth gate
  regressions (dot habu-habu-certified-words-84e84eaf): a certified/TRUSTED:/
  defer/axiom'd word executed at bare top level with fewer cells than its
  declared inputs fails closed with `hb: interpret stack underdepth:` + rc 70
  on both cold-prefix paths; positives pin exact/surplus depth, unguarded
  compiled calls, the unchecked-word boundary, catchable evaluate rejects, and
  the CHECK! probe. REPL recovery smoke: test/proc-pty.f PTY-UNDERDEPTH.
- `test/top-row-hook-test.f` — top-row hook engine regressions (dot
  habu-typed-top-engine-2b2e88aa): an in-process logging hook observes the
  exact (class, token, flags) event per interpret-dispatch token class
  (number/string/counted-string/char literals, tick, pre-BLR word calls,
  including certified min-in arity in the LFIND flags); `top-check@`
  round-trips; child forges prove `set-top-check` fails closed rc 70 with the
  named diagnostic on both cold-prefix paths and that raw stores into the
  sealed compile-preflight/top/snapshot hook band trap
  `ENGINE-ERROR:SEAL-VIOLATION` while both one-cell neighbors stay writable;
  custom checker-hook reinstall runs compile preflight, and a missing preflight
  emits exactly one LF-terminated diagnostic with no trailing byte. The
  no-binary recovery gate reruns this subprocess contract against its private
  Gforth-built `hb-stdin`, not only the native engine.
- `test/compile-preflight-recovery.f` — bootstrap-owned native/raw recovery fixture
  run explicitly by `tools/bootstrap.sh`: a missing compile-preflight inside
  `catch` + `evaluate` returns code 70, restores the interrupted package scope,
  resynchronizes the checker package scope, and leaves checked compilation usable.
- `test/top-row-warn-test.f` — tier-1 top-row tracker warning regressions (dot
  habu-typed-top-checker-82cf8b84): child probes assert p1 `' FOO2 execute`,
  p2 `0 0 catch`, and p3 `s" abc" + .` each emit exactly one `hb: top-row:`
  warning, that the eval-fixture idiom, the `CHECK!` probe, a mid-stream
  `TRUSTED:` shim, and a `0 set-check` window stay quiet, and that the row
  persists across `require`.
- `test/xt-effect-test.f` — xt<effect> value regressions (dot
  habu-typed-top-xt-096a8f1b): child candidate probes assert `['] W` retypes to
  xt<effect(W)> so `['] A execute`/`['] SP catch`/`['] SP is ACT` fit-certify and
  their misfits reject, an unsafe-definer tick (`['] deflinear`) rejects, and a
  non-xt-consumer sink (`['] A +`) keeps the plain xt cell; plus the tier-1
  `' FOO2 execute` underflow warning stays and the pre-armed tier-2 `0 0 catch`
  warning is pinned.
- `test/xt-cell-test.f` — xt<effect> storage cells (dot
  habu-typed-xt-storage-ddad4af8): `TYPED-VARIABLE HK [ n -- n ]` declares a
  persistent monomorphic code cell; a typed store `[: W ;] HK !` fit-checks W's
  certified effect against E (wrong-effect rejects), `HK @ execute` fit-checks the
  row (wrong-row rejects, row-poly surplus certifies), live store/fetch/execute
  and re-store round-trip, malformed quotation types reject, and a stage-3 pin
  asserts a raw `variable @ execute` still launders (flip owned by
  habu-checker-exec-of-5923c543).
- `test/effect-read-api-test.f` — checker effect-read export API + negative
  regression (dot habu-expose-checker-effect-95e853eb): a cold-prefix file
  resolves `EFFECT-QUERY` / `EFFECT-DIN-N` / `EFFECT-DOUT-N` / `EFFECT-DIN-FAM` /
  `EFFECT-DOUT-FAM` and asserts a certified word's din/dout arity + per-position
  family class (scalar/pointer/gray), unknown-word and out-of-range edges;
  renaming or removing any entry breaks the load loudly (rc 70).
- `test/seal.f` — friend-arena seal regressions: one negative forge per guarded
  PROT-GUARD sink (`!`/`c!`/`+!`/`atomic!`/`atomic-add`/`atomic-cas` plus the
  `read`/`ioctl`/`poll`/`readlink`/`stat64`/`lstat64`/`getdirentries64`/`mmap`
  syscall buffers, each exercising its own guard register) traps with exit
  `ENGINE-ERROR:SEAL-VIOLATION`, the latch is one-way, free holes stay writable, and
  post-seal language features still update protected cells via engine primitives.
  `patch32`/`snap-rebase` are compiler-internal and hand-review only (noted in
  the file).
- `test/aot-wid-suite.f` — protected-WID boot-integration regression (TFAM
  2b-v(f)): spawns `test/aot-wid-build.f` to build a throwaway engine with two
  baked protected word-list ids (300 and 70000), then proves those ids are
  restored at startup before batch input — read back from `PROT-WID-OFF`, `WIDN`
  advanced past them, publishing into either exits 84 on both stdin and `--load`,
  an ordinary define still exits 0, and the shipped engine protects neither id.
  Locks the load-bearing startup-restore hook `EM-AOT-RESTORE-HOOK-INIT` in
  `src/habu/habu2.f`.
- `test/aot-wid-build.f` — builder helper for the suite above: reads
  `src/habu/stdin.f`, fails closed if it no longer ends with the trailing top-level
  `GO` call, drops that call, and appends a `PWID-GO` that bakes the two protected
  word-list ids into the AOT registry via `aot-capture.f ACAP-PWID-PUT`; the rest
  reuses `tools/build-fixpoint.f`. No production source is touched.
- `test/owner-wid-emitter.f` — test-image-only cold emitter hook that drives the
  unpublished owner-pair mutator through exact capacity and atomic rejection.
- `test/owner-wid-source.f` — canonical owner package source shared by the AOT
  cold prefix and snapshot keep surface.
- `src/habu/owner-wid-emit-seal.f` — post-xref erasure of every build-time
  owner-registry label, hook, and mutable emitter capability.
- `test/owner-wid-image.f` / `test/owner-wid-doctor.f` /
  `test/owner-wid-child.f` / `test/owner-wid-internal.f` — native AOT/snapshot
  image builder, malformed-image doctor, process-isolated build worker, and
  owning parent runner for the persisted owner-registry proof.
- `test/owner-wid-build-forge.f` — negative package-reopen fixture proving the
  exact owner proof builder cannot be redirected to arbitrary source.
- `test/owner-wid-state.f` / `test/owner-wid-eval.f` / `test/owner-wid-call.f` /
  `test/owner-wid-private-call.f` — read-only role/capacity assertions plus
  hostile hidden-mutation and persisted-private-word fixtures.
- `test/owner-wid-guard.f` — load-head guard the owner-wid suites require so a
  standalone engine invocation dies fast with a named message and rc instead of
  misleading missing-build-context failures.
- `test/owner-wid-role-swap.f` — negative checker fixture: a sibling nominal
  locator-index fed where another is expected must reject, driven against the
  built AOT and snapshot engines by `test/owner-wid-child.f`.
- `test/owner-wid-snapshot.f` — snapshot-writer adversarial suite: builds a
  poisoned snapshot and proves the persisted return-stack window is zeroed, and
  builds a close-failing snapshot and proves the writer fails closed.
- `test/owner-wid-snapshot-poison.f` / `test/owner-wid-snapshot-close-fail.f` —
  builder-only fixtures injected into the snap source that plant return-stack
  canaries and arm the test-only `SNAP-CLOSE-SEAM` before `SNAPGO`.
- `test/wide-store-seal.f` — generated checked W=2 ADT store forges proving
  ordinary storage and first/later-cell protected-band intersections: zero-valued
  payload/tag attempts against the seal latch must trap `ENGINE-ERROR:SEAL-VIOLATION`
  before any protected mutation.
- `test/protection-span.f` — package-scoped raw-write forges proving interval
  overlap at the compiler transaction lower/upper boundaries, unaligned scalar
  and atomic writes, syscall-reported lengths, fixed mappings, and address-wrap
  rejection; exact half-open neighbors remain writable.
- `test/lower-txn-protection.f` — package-scoped immediate probes proving the
  dynamically mapped lowering transaction is protected throughout pass-2
  replay across syscalls, fixed mappings, FFI writable extents, and snapshot
  relocation, then fully unmapped with cleared state after publication.
- `test/lower-txn-large.f` — package-scoped checked source generator proving a
  valid certificate larger than the retired 64-KiB cap compiles and executes
  through the dynamically sized lowering transaction.
- `test/seal-absence.f` — Gforth stage0 absence-parity fixture: scans
  `bootstrap/cg/forth.fs` and fails closed if any pinned guard-bypass surface
  (atomics, snap-rebase, extended syscalls, `CHECKER-*` mutators) appears on a
  code line without a `PROT-GUARD`; it also pins the present span guards,
  seal-token boundary, package engine, and protected-WID reopen guard.
  In-memory self-proofs cover reject, guard-escape, and comment-only cases.
- `test/seal-package.f` — sealed system-package regressions (TFAM 2b-ii): child
  forges prove post-seal user source cannot open/reopen `package TFAM`/`TYPE`/
  `MATCH` nor define a qualified word into one (`: TFAM:tail ...`),
  case-insensitively, fail-closed with exit `ENGINE-ERROR:SEAL-PACKAGE`; ordinary packages
  and qualified defs still compile, and a trailing-colon ordinary name is never
  treated as qualified. Covers both `--load` and stdin cold-prefix entry paths.
- `test/engine-error-package.f` — package-scoped engine-failure ABI regressions:
  native/recovered child exits for codes 86–88 and post-seal checker-bridge
  success/fail-closed lookup corruption.
- `test/pre-trust-defer.f` — capability + fail-closed regressions for the
  pre-trust defer pending table (dot habu-engine-pre-trust-77410827): copies the
  src tree, patches the copy, and boots the engine-under-test with CWD there.
  Positive case proves capture -> drain -> checked `is` -> 42 round-trip;
  overflow at declaration exits 72; blanking the bare DRAIN-PRETRUST drain
  token leaves the prefix's own TFAM checker-hook defers undrained at
  SEAL-CAPTURE, exit 73 naming TFAM-RESOLVE-XT.
- `test/catch-frame.f` — a caught throw restores the complete caller execution
  frame (dot habu-restore-complete-exec-abb8baca): in-process asserts that a
  checked `r>`/`?do` index and the return/loop-stack depths survive a throwing
  quotation and nested/repeated throws, plus child forges that a corrupt sentinel,
  adjacent-sentinel mutation, or out-of-range saved depth fails closed with
  ENGINE-ERROR:CATCH-STACK (87) before any restore. Native runs it against
  HABU_UNDER_TEST; tools/bootstrap.sh runs it against the recovered candidate.
- `test/c3-widen-test.f` / `test/c4-shadow-test.f` — checker regressions for
  narrow-to-wide integer widening and local shadowing of ordinary words.
- `test/gate-build-common.f` — checked helpers shared by native hb-build gate
  slices.
- `test/gate-hb-build-repl.f` — checked runner for `hb-build --repl` checks.
- `test/boot-pin-test.f` — regression: boot-prefix digest determinism, drift detection, CLI verify, and path-list consistency with habu2.f.
- `test/pointer-storage-test.f` — focused pointer-cell initialization, typed address, round-trip, verifier-effect, and source-isolation regressions.
- `test/icode-fixup-test.f` — checked ARM64 label-chain and reusable fixup-slot
  regressions covering mixed relocation kinds, interleaved labels, historical
  reuse beyond capacity, exact simultaneous capacity, and overflow diagnostics.
- `test/engine-size-test.f` — exact emitted-engine region row and delta tests.
- `test/gate-pool.f` — bounded checked process pool used by native gate runners.
- `test/gate-pool-test.f` — focused fork-backed pool worker coverage.
- `test/gate-pool-orphan-test.f` — regression: pool workers reaped on parent death.
- `test/run.f` — native test suite entry run directly by `bin/hb`.
- `test/run-lib.f` — side-effect-free resident native test suite implementation.
- `test/cal-spin.f` — fresh-child calibration probe: measures the fixed gate spin in a freshly spawned process so the perf verdict's post-run calibration lands on a performance core, not the drifted driver's core.
- `test/run-lib-test.f` — manual-standalone coverage for the DGX Spark host profile mapping/detection and the fresh-child calibration probe.
- `test/run-support.f` — minimal scheduler support for starting external phases before resident setup.
- `test/run-resident.f` — late-loaded resident scheduler that forks phase workers without loading every phase library.
- `test/run-shared-stdlib.f` — parent-loaded stdlib setup inherited by forked stdlib workers.
- `test/run-worker.f` — fork-worker dispatcher that routes a resident phase to its owned support file.
- `test/run-worker-stdlib.f` — resident stdlib/check-cli/lint/tail phase support and dispatch.
- `test/run-worker-engine.f` — resident engine repair/fixture/runtime/validation phase support and dispatch.
- `test/run-worker-diag.f` — resident checker-diagnostics phase support and dispatch.
- `test/run-worker-diag-all-strict.f` — resident SARIF-backed diagnostics phase.
- `test/run-worker-dict.f` — resident dictionary/checker phase support and dispatch.
- `test/run-worker-debug.f` — resident prop/debug phase support and dispatch.
- `test/run-worker-aot.f` — resident positive AOT phase support.
- `test/run-worker-aot-neg.f` — resident negative AOT closure phase support.
- `test/run-files.f` — file sets that key native test suite caches.
- `test/gate-common.f` — thin entry wrapper for native gate helper definitions.
- `test/gate-common-lib.f` — side-effect-free native test-suite helper definitions.
- `test/candidate-runtime.f` — exact-candidate resident runtime source probes.
- `test/runtime-subject.f` — fork-isolated exact-candidate source capture adapter.
- `test/candidate-validation.f` — digest-exact resident candidate validation worker.
- `test/candidate-validation-test.f` — validation batching and retained-boundary regression.
- `test/gate-validation-worker.f` — candidate/baseline validation evidence runner.
- `test/gate-process-child.f` — fresh-process exec/fork telemetry inheritance fixture.
- `test/gate-stdlib.f` — thin entry wrapper for lint/stdlib gate slices.
- `test/gate-tail-process.f` — focused resident-fork runner and load-scaled
  nominal 10-second ratchet for the stdlib process-boundary tail.
- `test/tail-ratchet.f` — exact per-member direct/subject process-count,
  elapsed-time ratchet, and shared load-scaled tail budget policy.
- `test/gate-stdlib-lib.f` — side-effect-free lint/stdlib gate harness helpers.
- `test/gate-stdlib-cases.f` — executable lint/stdlib suite declarations.
- `test/gate-engine.f` — thin entry wrapper for engine/public-hb gate slices.
- `test/gate-engine-lib.f` — side-effect-free engine/public-hb gate definitions.
- `test/gate-diagnostics.f` — thin entry wrapper for checker diagnostic slices.
- `test/gate-diagnostics-lib.f` — side-effect-free checker diagnostic gate definitions.
- `test/gate-diagnostics-all-strict-lib.f` — SARIF-backed checker diagnostic slice.
- `test/gate-diagnostics-entry-lib.f` — checker diagnostic CLI dispatch definitions.
- `test/gate-dictionary.f` — thin entry wrapper for dictionary/checker contracts.
- `test/gate-dictionary-lib.f` — side-effect-free dictionary/checker contract definitions.
- `test/gate-debug.f` — thin entry wrapper for prop/debug checks.
- `test/gate-debug-lib.f` — side-effect-free prop/debug gate definitions.
- `test/gate-build-hbb.f` — in-process checked hb-build helpers for positive AOT gate coverage.
- `test/gate-build-size.f` — committed candidate binary size ratchet (per-target baselines, fail-closed on growth).
- `test/gate-size-attribution-test.f` — committed per-region byte-attribution manifest + gate: reconstructs the whole file from the committed macOS regions, checks the distance-to-page-floor, and couples each target's committed total to the live installed engine (fail-closed on drift); VALIDATE reconciles a captured map against its engine.
- `test/run-result-cache.f` — per-phase content-keyed PASS-stamp store for the native gate result cache.
- `test/run-result-cache-test.f` — fixtures for result-cache hit/miss/invalidation and red-never-cached rules.
- `test/run-budget-cal-test.f` — fixtures for the startup spin-probe budget calibration and clamping.
- `test/run-rerun-failed-test.f` — fixtures for --rerun-failed red-phase list persistence, parsing, and phase-skip guard.
- `test/golden.f` — byte-exact golden-file assertions for diagnostic output, with --update-golden and temp-path redaction.
- `test/golden-test.f` — fixtures for the golden-file update/compare/drift/redaction mechanism.
- `test/gate-aot-positive.f` — thin entry wrapper for AOT positive checks.
- `test/gate-aot-positive-lib.f` — side-effect-free AOT positive gate definitions.
- `test/gate-aot-negative.f` — thin entry wrapper for AOT rejection checks.
- `test/gate-aot-negative-lib.f` — side-effect-free AOT rejection gate definitions.
- `test/gate-runner-lib.f` — side-effect-free phase dispatch definitions for native test runners.
- `test/gate-runner-support.f` — side-effect-free support bundle for focused runner-entry invocations.
- `test/gate-runner-entry.f` — tiny CLI entry for focused native runner dispatch.
- `test/gate-runner-entry-test.f` — standalone-load regression: spawns the documented `gate-runner-support`+`gate-runner-entry` closure and asserts it reaches GR-USAGE (rc 64), proving the whole require chain loads under the raised dictionary cap.
- `test/load-reject-diag-test.f` — spawn regression: rejecting `--load` paths must exit 70 with named stderr; signature-bearing and signatureless compile-time immediates reject before side effects with one structured repair packet, while top-level loaders, runtime `included`/`required`/`provided`, modeled immediates, and audited trusted immediate bodies stay live.
- `test/gate-stdlib-inline-lib.f` — in-process stdlib gate slice dispatcher for resident runner forks.
- `test/gate-stdlib-tool-base-ready.f` — resident-runner sentinel that marks the common stdlib tool base as already loaded.
- `test/gate-stdlib-lint-tools.f` — in-process lint-tools group body loaded after shared setup.
- `test/prop-test-core.f` — reusable property-based checker-soundness runner.
- `test/prop-test.f` — CLI entry for property-based checker-soundness test.
- `test/engine-suite.f` — native engine behavior suite.
- `test/extent-substrate-probe.f` — standalone decision-record demo (habu-choose-extent-nominal): TFAM parametric families type idx<#M> with two extents distinct and host BTC-7's #B*#T product structure; run over stdin, see docs/extent-substrate.md.
- `test/type-decl-suite.f` — behavior suite for the TYPEFAMILY/SUMTYPE declaration grammar (positives, negatives, rollback, multi-error, diagnostics).
- `test/deftype-suite.f` — behavior contract for the `DEFTYPE` surface (lib/type/deftype.f): same-nominal accept, other-nominal/generic-int reject, explicit converters, converter no-launder, demanded-input direction, package scoping (same name in two packages stays distinct), and snapshot-persist survival.
- `test/deftype-dup-bad.f` — child-process negative fixture: a duplicate `DEFTYPE` in one package is refused fail-closed (exit 67, "duplicate family").
- `test/cast-suite.f` — positive behavior contract for the `CAST:` checked retype declarer (src/core/roles.f + checker.f CAST-PEND window): empty-body and guarded nominal casts, runtime value pass-through, guard throw, parametric round-trip and generic projection, and checked-caller certification against the published row.
- `test/cast-negative-suite.f` — reject contract for `CAST:`: E-CAST-ARITY/E-CAST-CLASS/E-CAST-FAM named rejects, identity-certification failures (net-stack and input-consuming bodies), and the unsafe-token reject of `cast:` inside a checked body.
- `test/type-ctor-suite.f` — behavior suite for generated sum constructors (arity-0 publication, payload rejects, parametric/linear gating, package restore).
- `test/type-linear-suite.f` — whole-bundle linear accounting suite (linear construction/minting/flow accepts; copy/drop/transport/local/unconsumed rejects).
- `test/type-match-suite.f` — checked MATCH eliminator suite (exhaustiveness, payload refinement, branch joins, linear consumption, depth fail-closure, scope, CASE-interleave pins).
- `test/lower-cert.f` — package-scoped canonical lowering-certificate producer regressions covering source binding, width rows, fetch descriptors, and guard-domain evidence.
- `test/type-layout-lower-pending.f` — width-aware lowering fixtures retained for negative and boundary coverage after the immutable transaction switchover.
- `test/layout-buffer.f` — generative closed-layout storage, pointer-provenance rejection, bounds, stride, zero-image, and rollback regressions.
- `test/typed-storage-test.f` — TYPED-VARIABLE/TYPED-BUFFER convenience definers: nominal/layout/typed-pointer store-fetch round-trips, cross-family/bounds/overflow/duplicate/rollback rejections, and the distinct LAYOUT-BUFFER-positive / raw-variable-negative capability pins.
- `test/layout-buffer-depth.f` — entry for the executable maximum-include-depth plus generated-accessor evaluation regression.
- `test/layout-buffer-depth-0.f` — evaluator-depth fixture link 0.
- `test/layout-buffer-depth-1.f` — evaluator-depth fixture link 1.
- `test/layout-buffer-depth-2.f` — evaluator-depth fixture link 2.
- `test/layout-buffer-depth-3.f` — evaluator-depth fixture link 3.
- `test/layout-buffer-depth-4.f` — evaluator-depth fixture link 4.
- `test/layout-buffer-depth-5.f` — evaluator-depth fixture link 5.
- `test/layout-buffer-depth-6.f` — evaluator-depth fixture link 6.
- `test/layout-buffer-depth-7.f` — deepest evaluator fixture using a named capacity constant.
- `test/layout-buffer-forge.f` — armed child-process regression proving the layout-buffer checker capability is absent from user source.
- `test/layout-valid-w1-bad.f` — child-process W1 enum invalid-tag fetch regression (exit 85 before publication).
- `test/layout-valid-product-bad.f` — child-process nested product-field enum invalid-tag fetch regression.
- `test/layout-valid-hook-forge.f` — armed child-process proof that the erased typed-fetch record hook cannot be replaced.
- `test/layout-valid-walk-forge.f` — armed child-process proof that the erased validator recursion hook cannot be replaced.
- `test/layout-valid-desc-forge.f` — armed child-process proof that the erased descriptor bridge cell cannot be replaced.
- `test/layout-valid-guard-base.f` — shared low-level nested-SUM metadata and raw-image boundary for validator guard tests.
- `test/layout-valid-guards.f` — valid active nested and inactive garbage-payload typed-fetch regressions.
- `test/layout-valid-active-bad.f` — armed child-process active nested invalid-tag regression.
- `test/layout-valid-root-bad.f` — armed child-process invalid root-tag regression.
- `test/layout-valid-growth.f` — forty-level nested-SUM fetch proving descriptor, environment, and guard arena growth.
- `test/type-family-suite.f` — behavior suite for TFAM/SUMV/layout/SCHEMA plus shared field schema validation, nested transactions, policies, and reflection.
- `test/type-family-rollback-suite.f` — behavior suite for depth-safe candidate/scope rollback including committed shared field and string-pool restoration.
- `test/type-export-suite.f` — checker-level EXPORT alias suite (CHECKER-EXPORT): cross-package alias fidelity, defer/control-flag copy, every reject class, scope/candidate rollback of alias rows.
- `test/export-package.f` — EXPORT keyword engine-contract regressions: child forges pin dual-name execution, the top-level hb-build directive no-op, generated-ctor re-export, DNAME-WIDE parity, and every reject exit status.
