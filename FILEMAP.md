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
- `docs/type-families.md` — generic lowercase type-family/ADT design plan.
- `docs/census-switchover.md` — site-level inventory for the post-TFAM switchover: sentinel-return conventions to migrate to option/result, legacy enum clusters, value-record/PTX-IR products, ADT-dischargeable trust rows, and the wave-ordered migration plan.
- `docs/gate.md` — native gate architecture, proof subjects, metrics, and
  process-boundary rules.
- `docs/kernel-principles.md` — roofline, the 3 bounds, the device's compute/memory roofs, and where each Habu kernel sits (apply before optimizing any kernel).
- `docs/parallel-agents.md` — map-reduce protocol for parallel dot execution.
- `docs/ptx.md` — Habu→PTX GPU DSL strategy and scope.
- `docs/ptx-sketch.md` — Habu→PTX v0 language spec.
- `docs/inference.md` — Habu→PTX local type inference: infer bodies, annotate the contract edge.
- `docs/autograd.md` — Habu→PTX reverse-mode autograd: AD as a syntactic reversal; verified gradients.
- `docs/eval-triton.md` — eval matrix: checked Habu-PTX vs real Triton on the Orin (install, reproduction, results).
- `docs/seed.md` — native seed trust root and no-binary recovery.
- `docs/swiftforth-task-api.md` — SwiftForth multitasking surface captured for
  Habu `TASK` package parity.

## Core Checker

- `src/core/util.f` — shared subset helpers.
- `src/core/bytes.f` — core byte-buffer helpers (`BYTE+`, `BYTE-COPY-LEN`,
  `BYTE-COPY`) loaded before stdlib/tool sources so low-level modules do not
  depend on `lib/string.f` order.
- `src/core/checker.f` — native stack-effect checker and verifier.
- `src/core/type-schema.f` — persistent type-schema node arena (package TFAM) referenced by families/variants/fields as schema roots.
- `src/core/type-family.f` — package-scoped type-family (TFAM), sum-variant (SUMV), product-field, and layout registries with snapshot persist.
- `src/core/render.f` — human/JSON diagnostics and signature recording.
- `src/core/sumtype.f` — TYPEFAMILY/SUMTYPE declaration grammar registering package-aware families, variants, and payload schemas.
- `src/core/roles.f` — audited nominal scalar role conversion words.
- `src/core/include.f` — checked source composition words (`include`, `included`) with dynamic `evaluate` isolated to `INCLUDE-EVALUATE`, plus the ordered source-composition event log (`EVENT-RECORD`, `EVENT-ON`/`DISCOVERY-ON`) that records include multiplicity and require/provided registry state, and `REQUIRE-SNAPSHOT`/`REQUIRE-RESTORE` giving the discovery pass a fresh require registry without disturbing warm-snapshot state.
- `src/core/structures.f` — early `BEGIN-STRUCTURE`, `+FIELD`, `CFIELD:`, and `END-STRUCTURE` layout DSL definitions.
- `src/core/structures-effects.f` — checker effect rows for the early structure defining words.
- `src/core/enums.f` — checked `ENUM+` and `ENUM4+` legacy numeric counter definers for named integer families.
- `src/core/exec-vector.f` — checked execution-vector support for `defer`/`is` runtime sentinels.
- `src/core/sha256.f` — standalone SHA-256, streaming file digest, and hex helpers.
- `src/core/type-family-sha.f` — installs the constructor package-name SHA-256 fallback hook (TF-SHA16) after sha256.f loads.
- `src/core/sha-check.f` — standalone SHA-256 self-test against FIPS-180 vectors.
- `src/core/check-hook.f` — default native source checker hook installation.
- `src/core/combinators.f` — legacy higher-order library words baked into
  `bin/hb` (audited unchecked boundary; new higher-order words are checked).

## Native Engine And Builders

- `src/habu/habu1.f` — primitive engine, dictionary, parser, and prim registry.
- `src/habu/habu2.f` — compiler/control-flow layer.
- `src/habu/driver-io.f` — shared fail-closed artifact writer for internal
  build drivers.
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
  kernels.
- `lib/ptx/ad-dag-test.f` — checked validation tests for PTX AD DAG overflow,
  underflow, unknown opcode, and non-singleton output rejection.
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

- `test/gate-stats.f` — checked append-only counter log and summary helpers for
  native gate duplicate-work RCA.
- `test/gate-stats-test.f` — focused fixture for gate stats event counting.
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
- `tools/hb-build-lib.f` — checked native AOT/REPL build CLI library.
- `tools/hb-build-direct-lints.f` — optional in-process lint hook adapter for
  hb-build gate callers that already loaded lint cores.
- `tools/hb-build.f` — Habu entrypoint for native AOT/REPL builds.
- `tools/hb-build-test.f` — checked fixture coverage for native REPL builds and
  hb-build boundary rejections.
- `tools/cli-run.f` — checked helpers for explicitly installed CLI fixture
  subprocesses.
- `tools/bootstrap-codegen-test.f` — native source regression for bootstrap codegen fail-closed contracts.
- `tools/imgdump.f` — native image dictionary dump and compare tool.
- `tools/imgdump-test.f` — checked fixture coverage for image dump compare mode.
- `tools/imagedisasm.f` — native raw image slice disassembler.
- `tools/imagedisasm-test.f` — checked fixture coverage for raw image disassembly.
- `tools/include-events-test.f` — checked fixtures for the source-composition event log and loader instrumentation.
- `tools/source-discovery.f` — whole-file source-composition discovery pass that lexes the entire token stream (colon bodies included), replays every literal loader form against a fresh require registry, and emits the ordered event artifact; dynamic paths, loader shadow/undefine/retirement, and unsupported openers reject fail-closed unless the entry is a declared dynamic-tail boundary.
- `tools/source-discovery-test.f` — checked fixtures for the whole-file discovery pass (ordering, multiplicity, dedup, fresh registry, colon-body capture, byte-exact spans, shared emitter, fail-closed rejection, dynamic-tail manifest boundary).
- `tools/dynamic-tail-manifest.f` — declared dynamic-tail boundary table (path + reason) consumed by the discovery pass; a listed file's dynamic/retired loader forms are tolerated instead of rejected.
- `tools/event-closure-lib.f` — ordered transitive source-composition closure list built by replaying the discovery pass breadth-first over the event log.
- `tools/event-closure-test.f` — checked fixtures for the closure list (order, dedup, transitive descent, provided/missing exclusion, colon-wrapped deps) and closure key sensitivity.
- `tools/ptx/saxpy.f` — CLI entrypoint that emits the M3 SAXPY PTX kernel.
- `tools/ptx/saxpy-test.f` — checked fixture for the PTX SAXPY encoder output.
- `tools/ptx/ptxas-smoke.f` — Orin-only checked smoke that emits SAXPY PTX,
  runs `ptxas`, and removes generated `.ptx`/`.cubin` artifacts.
- `tools/ptx/saxpy-cg.f` — checked SAXPY kernel body run through the PTX codegen
  vocabulary.
- `tools/ptx/saxpy-v4-tail-device-test.f` — Orin device proof that checked v4
  SAXPY residual scalar lanes are correct for `n=4,5,7,1000003`.
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
- `tools/ptx/sum-device-cg.f` — single-kernel SUM_ROWS emit (one module header)
  so ptxas assembles it for the Orin sum device golden; same body as sum-cg.f.
- `tools/ptx/zed-device-suite.f` — Orin device proof of the collective fix:
  emits SUM_ROWS / softmax forward / softmax backward PTX with the branch engine,
  ships via the ssh harness, remote ptxas-assembles, and launches the committed
  launchers on the Orin comparing the CPU reference. HABU_ZED-gated.
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
- `lib/ptx/collective.f` / `lib/ptx/collective-test.f` — tile-DSL row and
  collective vocabulary (M6) plus the checked stable-softmax proof.
- `lib/ptx/gemm-checked-test.f` / `lib/ptx/gemm-checked-neg-test.f` — checked
  tiled GEMM data-flow positive and negative regressions.
- `lib/ptx/cg.f` / `lib/ptx/cg-vec.f` / `lib/ptx/cg-collective.f` /
  `lib/ptx/cg-matmul.f` / `lib/ptx/cg-attention.f` — PTX codegen emit-mode
  lowering for tile ops: scalar, vectorized v4, row/collective, the
  register-blocked SGEMM, and the fused attention kernel.
- `lib/ptx/cg-matmul-naive.f` — the naive one-element-per-thread SGEMM baseline
  kernel (MMN) that the GEMM benchmark measures the register-blocked tile against.
- `lib/ptx/cg-mma.f` — TF32 tensor-core (mma.sync.aligned.m16n8k8) tiled GEMM
  (MMM): same 64x64 block and cp.async double-buffered staging as the register-
  blocked SGEMM, the 4x4 fma micro-tile swapped for warp-level MMA tiles (the
  compute-roof beat-Triton lever).
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
- `tools/ptx/matmul-device-test.f` — committed device-correctness regression
  for the tiled SGEMM kernel.
- `tools/ptx/profile-test.f` / `tools/ptx/bench-test.f` — focused coverage for
  PTX benchmark/profile math and configuration.
- `lib/test.f` — public checked test framework interface: assertions plus
  the `TEST:*` suite/group/test package facade.
- `lib/test/assert.f` — checked assertion primitives used by test fixtures.
- `lib/test/budget.f` — load-aware test timeout budgets: `T-BUDGET-MS` scales nominal budgets by the gate-exported `HB_LOAD_PCT` cal-factor, clamped to at most 3x.
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
- `lib/test/src-shape.f` — shared `package SHAPE` source-shape assertions: LOAD
  auto-sizes an OS-backed buffer to a whole source file (fail-closed CHECK-FIT
  diagnostic above the ceiling), then HAS?/MUST-HAVE/MUST-LACK/COUNT=/COUNT
  assert its shape. Removes the divergent per-test CAP/BUF/READ-ALL machinery.
- `lib/test/src-shape-test.f` — focused coverage for the source-shape helper:
  fail-closed CHECK-FIT, auto-sized LOAD roundtrip, and HAS?/COUNT assertions.
- `tools/ptx/cuda-launch.f`, `tools/ptx/softmax-launch.f`, and
  `tools/ptx/softmax-gradcheck.f` — Orin CUDA Driver proofs for launch,
  softmax, and finite-difference gradient checking.
- `tools/ptx/profile.f`, `tools/ptx/bench.f`, `tools/ptx/bandwidth-lib.f`,
  `tools/ptx/bandwidth.f`, `tools/ptx/bandwidth-v4.f`, and
  `tools/ptx/fusion-compare.f` — reusable Orin kernel profile metrics, generic
  CUDA Driver launch plus CUDA-event device timing, scalar/v4 SAXPY bandwidth,
  and fused-vs-unfused kernel comparison for the Habu-PTX column.
- `tools/ptx/gemm-bench.f` — CUDA-event GEMM benchmark: times the naive (MMN)
  and register-blocked (MM) SGEMM kernels on square shapes for the
  GEMM-vs-Triton baseline recorded in `docs/eval-triton.md`.
- `tools/ptx/mma-probe.f` — single-warp TF32 `mma.sync.aligned.m16n8k8` fragment-
  layout isolation proof: verifies ONE MMA element-exact vs a host matmul before
  any tiling (the course's #1 "correct in NumPy, garbage on device" guard).
- `tools/ptx/mma-gemm-check.f` — device-correctness of the full K-looping TF32
  mma.sync GEMM kernel (MMM) element-exact vs a host matmul at 64^3 and 128^3
  (staging + accumulation + the warp/D-fragment store mapping).
- `maki/README.md` / `maki/STATUS.md` — Maki framework overview and current
  verification status outside the Habu trust root.
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
- `tools/build-fixpoint-test.f` — checked fixture coverage for the self-rebuild fixpoint driver.
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
- `tools/lint/shadow-lint.f` — rejects toolchain definitions that shadow
  engine PRIM names.
- `tools/lint/clobber-lint.f` / `tools/lint/clobber-lint-test.f` —
  register-clobber analysis for BL-able emitter routines and its regressions;
  its negative syscall-scratch fixture file is a committed filemap-lint
  exclusion.
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
- `tools/check-all-errors-core.f` — reusable all-errors checker core; keeps
  per-definition checker runs as the diagnostic isolation boundary.
- `tools/check-all-errors.f` — CLI wrapper for all-errors checking.
- `tools/check-all-errors-test.f` — checked fixture coverage for all-errors checking.
- `tools/checked-boundary-lint-core.f` — reusable unchecked-boundary scanner core for checker and CLI paths.
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
- `tools/trusted-inventory-test.f` — checked fixture coverage for the trusted-inventory ratchet.
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
- `tools/string.f` — shared checked byte-string helper library.
- `lib/string-test.f` — focused coverage for checked string helpers.
- `lib/json-write.f` — checked emit-only JSON writer vocabulary for fixtures and native tools.
- `lib/json-write-test.f` — focused coverage for JSON writer escaping, structure, and errors.
- `lib/json-read.f` — checked zero-allocation JSON pull/cursor parser complementing the writer.
- `lib/json-read-test.f` — focused coverage for JSON parser tokens, escapes, structure, errors, and round-trip.
- `lib/memory.f` — checked OS-backed byte buffer allocation helpers.
- `lib/memory-test.f` — focused coverage for memory allocation and 64K buffer spans.
- `lib/vector.f` — checked growable cell-vector helpers backed by OS memory.
- `lib/vector-test.f` — focused coverage for vector growth, bounds, typed pointer storage, and iteration.
- `lib/layout/box.f` — boxed-layout record arena (TFAM 16): bump-allocated tag+payload heap records over the mmap allocator, arena free-all ownership.
- `lib/layout/box-test.f` — focused coverage for box record alloc, tag/payload round-trip, chunk growth, and arena reset.
- `lib/adt/option.f` — the shared `option<T>` sum family (some value / none), the checked replacement for -1/sentinel returns (switchover wave A); require before consumers.
- `lib/adt/result.f` — the shared `result<ok,err>` sum family (ok value / err value), the checked replacement for value+flag/rc-plus-value returns where the flag distinguishes DIFFERENT errors (switchover wave B); require before consumers.
- `lib/adt/result-test.f` — focused proof that result<ok,err> constructs (RESULT:OK/ERR), MATCHes both arms, and rejects swapped ok/err payload types.
- `lib/ffi-abi.f` — checked target-independent AAPCS64 FFI calls and marshalling: x0-x8, d0-d7, stack-spill, out-params, kernelParams, and int/float-return trampolines.
- `lib/ffi-abi-test.f` — focused coverage for portable FFI marshalling without dynamic loader slots.
- `lib/ffi.f` — checked dynamic loading layer over the FFI ABI: dlopen/dlsym through target-provided loader slots.
- `lib/ffi-test.f` — focused coverage for FFI dlopen/dlsym, fixed-arity calls, C-string marshalling, FP args/returns, x8, and stack spill.
- `lib/task.f` — checked pthread-backed tasking: task TCBs, per-task region
  re-entry, task-local user variables, halt/join teardown, and mutex facilities.
- `lib/task-test.f` — focused coverage for two task workers, atomic shared
  progress, mutex-protected increments, and cooperative halt/join cleanup.
- `lib/float.f` — checked decimal string to IEEE-double parsing (STR>FLOAT) with power-of-ten scaling.
- `lib/float-test.f` — focused coverage for STR>FLOAT sign, fraction, exponent, and rejection cases.
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
- `lib/codesign.f` / `lib/codesign-test.f` — checked executable promotion and
  ad-hoc signing helpers and their coverage.
- `lib/fs.f` / `lib/fs-test.f` — checked filesystem helpers (walks, reads,
  stat) and their coverage.
- `lib/fs-mutate.f` / `lib/fs-mutate-test.f` — checked filesystem mutation
  helpers (mkdir, remove, rename, cleanup) and their coverage.
- `lib/map.f` / `lib/map-test.f` — fixed-capacity open-addressed string-key
  map layout and its coverage.
- `lib/process.f` / `lib/process-test.f` — checked process helpers and their
  coverage.
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
- `test/atomics-smoke.f` / `test/run-in-stack-smoke.f` — tasking primitive
  smoke tests for atomics and the in-stack runner.
- `test/seal.f` — friend-arena seal regressions: one negative forge per guarded
  PROT-GUARD sink (`!`/`c!`/`+!`/`atomic!`/`atomic-add`/`atomic-cas` plus the
  `read`/`ioctl`/`poll`/`readlink`/`stat64`/`lstat64`/`getdirentries64`/`mmap`
  syscall buffers, each exercising its own guard register) traps with exit
  `E-SEAL-VIOLATION`, the latch is one-way, free holes stay writable, and
  post-seal language features still update protected cells via engine primitives.
  `patch32`/`snap-rebase` are compiler-internal and hand-review only (noted in
  the file).
- `test/seal-absence.f` — Gforth stage0 absence-parity fixture: scans
  `bootstrap/cg/forth.fs` and fails closed if any pinned guard-bypass surface
  (atomics, snap-rebase, extended syscalls, `CHECKER-*` mutators, package
  intrinsic) appears on a code line without a `PROT-GUARD`, and pins the present
  `PROT-GUARD`/`EMIT-SEAL-FRIEND` seal machinery so a mirrored guard cannot be
  silently deleted. In-memory self-proofs cover the reject, guard-escape, and
  comment-only cases.
- `test/seal-package.f` — sealed system-package regressions (TFAM 2b-ii): child
  forges prove post-seal user source cannot open/reopen `package TFAM`/`TYPE`/
  `MATCH` nor define a qualified word into one (`: TFAM:tail ...`),
  case-insensitively, fail-closed with exit `E-SEAL-PACKAGE`; ordinary packages
  and qualified defs still compile, and a trailing-colon ordinary name is never
  treated as qualified. Covers both `--load` and stdin cold-prefix entry paths.
- `test/c3-widen-test.f` / `test/c4-shadow-test.f` — checker regressions for
  narrow-to-wide integer widening and local shadowing of ordinary words.
- `test/gate-build-common.f` — checked helpers shared by native hb-build gate
  slices.
- `test/gate-hb-build-repl.f` — checked runner for `hb-build --repl` checks.
- `test/boot-pin-test.f` — regression: boot-prefix digest determinism, drift detection, CLI verify, and path-list consistency with habu2.f.
- `test/gate-pool.f` — bounded checked process pool used by native gate runners.
- `test/gate-pool-test.f` — focused fork-backed pool worker coverage.
- `test/gate-pool-orphan-test.f` — regression: pool workers reaped on parent death.
- `test/run.f` — native test suite entry run directly by `bin/hb`.
- `test/run-lib.f` — side-effect-free resident native test suite implementation.
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
- `test/gate-stdlib.f` — thin entry wrapper for lint/stdlib gate slices.
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
- `test/load-reject-diag-test.f` — spawn regression: a rejecting `--load` (direct, require-chain, checked-body) must exit 70 WITH a named stderr diagnostic, never silently.
- `test/gate-stdlib-inline-lib.f` — in-process stdlib gate slice dispatcher for resident runner forks.
- `test/gate-stdlib-tool-base-ready.f` — resident-runner sentinel that marks the common stdlib tool base as already loaded.
- `test/gate-stdlib-lint-tools.f` — in-process lint-tools group body loaded after shared setup.
- `test/prop-test-core.f` — reusable property-based checker-soundness runner.
- `test/prop-test.f` — CLI entry for property-based checker-soundness test.
- `test/engine-suite.f` — native engine behavior suite.
- `test/type-decl-suite.f` — behavior suite for the TYPEFAMILY/SUMTYPE declaration grammar (positives, negatives, rollback, multi-error, diagnostics).
- `test/type-ctor-suite.f` — behavior suite for generated sum constructors (arity-0 publication, payload rejects, parametric/linear gating, package restore).
- `test/type-linear-suite.f` — whole-bundle linear accounting suite (linear construction/minting/flow accepts; copy/drop/transport/local/unconsumed rejects).
- `test/type-match-suite.f` — checked MATCH eliminator suite (exhaustiveness, payload refinement, branch joins, linear consumption, depth fail-closure, scope, CASE-interleave pins).
- `test/type-layout-lower-pending.f` — staged TFAM 12 slice-3 width-aware lowering fixtures: real compile-subject words plus the per-op width-fact contract; standalone, not yet wired into a suite.
- `test/type-family-suite.f` — behavior suite for the package-scoped TFAM/SUMV/product/layout/SCHEMA registries.
- `test/type-family-rollback-suite.f` — behavior suite for the checker's depth-safe transactional candidate/scope rollback frames.
- `test/type-export-suite.f` — checker-level EXPORT alias suite (CHECKER-EXPORT): cross-package alias fidelity, defer/control-flag copy, every reject class, scope/candidate rollback of alias rows.
- `test/export-package.f` — EXPORT keyword engine-contract regressions: child forges pin dual-name execution, the top-level hb-build directive no-op, generated-ctor re-export, DNAME-WIDE parity, and every reject exit status.
