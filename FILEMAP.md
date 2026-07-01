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
- `src/core/render.f` — human/JSON diagnostics and signature recording.
- `src/core/roles.f` — audited nominal scalar role conversion words.
- `src/core/include.f` — checked source composition words (`include`, `included`) with dynamic `evaluate` isolated to `INCLUDE-EVALUATE`.
- `src/core/structures.f` — early `BEGIN-STRUCTURE`, `+FIELD`, `CFIELD:`, and `END-STRUCTURE` layout DSL definitions.
- `src/core/structures-effects.f` — checker effect rows for the early structure defining words.
- `src/core/enums.f` — checked `ENUM` and `ENUM4` defining words for named integer families.
- `src/core/exec-vector.f` — checked execution-vector support for `defer`/`is` runtime sentinels.
- `src/core/sha256.f` — standalone SHA-256, streaming file digest, and hex helpers.

## Native Engine And Builders

- `src/habu/habu1.f` — primitive engine, dictionary, parser, and prim registry.
- `src/habu/habu2.f` — compiler/control-flow layer.
- `src/habu/driver-io.f` — shared fail-closed artifact writer for internal
  build drivers.
- `src/habu/jit.f` — register JIT helpers.
- `src/habu/regalloc.f` — virtual stack register allocator.
- `src/habu/aot.f` — stripped AOT linker driver.
- `src/habu/build.f` — `hb-build --repl` bundle driver.
- `src/habu/maker.f` — generic maker-image build driver for `hb-build`.
- `src/habu/snap-lib.f` — checked snapshot writer definitions.
- `src/habu/snap.f` — snapshot writer entry point.
- `src/habu/stdin.f` — internal stdin/interactive engine builder.

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
- `lib/ptx/test-prelude.f` — require-only shared setup for PTX positive entry
  tests; suites list the entry tests, not this dependency bundle.
- `lib/ptx/process-test-prelude.f` — require-only process-boundary setup for
  PTX tests that must exercise a child process boundary.
- `lib/ptx/neg-test-lib.f` — require-only in-process helper for PTX semantic
  rejection tests that call the checker directly and capture diagnostics.
- `lib/ptx/launch.f` — checked PTX launch-contract helpers for row kernels
  (`rows > 0`, `cols > 0`, legal block, and `cols <= block`).
- `lib/ptx/launch-test.f` — checked fixtures for PTX launch-contract rejection.
- `lib/ptx/ir.f` — checked value-numbered PTX expression IR with constant
  folding, peephole canonicalization, CSE, and DCE live marking.
- `lib/ptx/ir-test.f` — checked value fixtures for PTX IR fold, peephole,
  CSE, DCE, and overflow rejection.
- `lib/ptx/ad-dag.f` — checked reverse-mode symbolic DAG builder for PTX row
  kernels.
- `lib/ptx/ad-dag-test.f` — checked validation tests for PTX AD DAG overflow,
  underflow, unknown opcode, and non-singleton output rejection.
- `src/arch/arm64/disasm.f` — native ARM64 subset disassembler used by
  `tools/jitdump.f` and `tools/imagedisasm.f`.
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
- `docs/macho.md` — Mach-O layout notes.

## Tools And Gates

- `test/gate-stats.f` — checked append-only counter log and summary helpers for
  native gate duplicate-work RCA.
- `test/gate-stats-test.f` — focused fixture for gate stats event counting.
- `tools/seed.f` — checked native seed installer, SHA verifier, smoke test, and fixpoint rebuild driver.
- `tools/seed-main.f` — CLI entrypoint for checked native seed recovery.
- `tools/seed-test.f` — focused coverage for seed SHA, install, signing, and smoke helpers.
- `tools/build-fixpoint.f` — checked native stage/stdin build driver; explicit
  `snap` builds warm snapshot candidates for cache/debug paths.
- `tools/check-core.f` — reusable Habu-native checked engine runner core.
- `tools/check.f` — thin CLI entrypoint for the checked engine runner.
- `tools/check-main.f` — no-include checked engine entry for checker CLI reuse.
- `tools/check-test.f` — checked fixture coverage for the native check runner.
- `tools/sha256-file-test.f` — checked fixture coverage for streaming SHA-256 helpers.
- `lib/content-key.f` — checked manifest-hash builder for content-addressed gate caches.
- `lib/content-key-test.f` — checked fixture coverage for content-key stability and invalidation.
- `tools/hb-cli-contracts-test.f` — checked coverage for `hb` startup and stdin-data contracts.
- `tools/hb-baseline-contracts-test.f` — checked public `bin/hb` baseline contract fixture.
- `tools/hb-build-lib.f` — checked native AOT/REPL build CLI library.
- `tools/hb-build-direct-lints.f` — optional in-process lint hook adapter for
  hb-build gate callers that already loaded lint cores.
- `tools/hb-build.f` — Habu entrypoint for native AOT/REPL builds.
- `tools/hb-build-test.f` — checked fixture coverage for native REPL builds and
  hb-build boundary rejections.
- `tools/warm-image-lib.f` — checked tool library that bakes warm snapshot images
  plus generated trust-sidecar sources from support files for feature tests.
- `tools/warm-image-gate-stats.f` — tiny gate-only adapter that wires
  warm-image events into `test/gate-stats.f` without making warm-image-lib depend
  on test code.
- `tools/warm-image.f` — CLI entrypoint for warm snapshot image baking.
- `tools/warm-image-test.f` — checked fixture coverage for warm image baking,
  support-source availability, and warm-image checker rejection.
- `tools/warm-run.f` — checked helpers for explicitly installed warm fixture
  subprocesses.
- `tools/bootstrap-codegen-test.f` — native source regression for bootstrap codegen fail-closed contracts.
- `tools/imgdump.f` — native image dictionary dump and compare tool.
- `tools/imgdump-test.f` — checked fixture coverage for image dump compare mode.
- `tools/imagedisasm.f` — native raw image slice disassembler.
- `tools/imagedisasm-test.f` — checked fixture coverage for raw image disassembly.
- `tools/ptx/saxpy.f` — CLI entrypoint that emits the M3 SAXPY PTX kernel.
- `tools/ptx/saxpy-test.f` — checked fixture for the PTX SAXPY encoder output.
- `tools/ptx/ptxas-smoke.f` — Orin-only checked smoke that emits SAXPY PTX,
  runs `ptxas`, and removes generated `.ptx`/`.cubin` artifacts.
- `tools/ptx/saxpy-cg.f` — checked SAXPY kernel body run through the PTX codegen
  vocabulary.
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
- `tools/ptx/redadd-cg.f` — raw-PTX emit driver for a `red.global.add.f32` kernel (each thread
  atomically adds 1.0 to out[0]); the scatter-add primitive reverse-mode fan-in adjoints need.
- `tools/ptx/redadd-device-test.f` — Orin device proof that `red.global.add.f32` assembles for
  sm_87 and accumulates correctly (256 atomic adds = 256.0); closes habu-ptx-ad-verify.
- `tools/ptx/sum-cg.f` / `tools/ptx/sum-launch.f` — checked direct row-sum
  codegen plus Orin device proof for BLOCK-SUM's reducer-local inactive-lane zero.
- `tools/ptx/sum1024-cg.f` — checked direct row-sum text fixture proving `%BLOCK
  1024` changes shared-memory size and reduction fold bounds.
- `tools/ptx/softmax-cg.f` / `tools/ptx/softmax-bwd-cg.f` — checked
  SOFTMAX-ROWS forward/backward emit drivers.
- `lib/ptx/ad-ir.f` / `tools/ptx/softmax-bwd-opt-cg.f` — AD-op-list to PTX-IR
  bridge plus closed-form SOFTMAX backward emitter for the saved-output path.
- `lib/test.f` — public checked test framework interface: assertions plus
  the `TEST:*` suite/group/test package facade.
- `lib/test/assert.f` — checked assertion primitives used by test fixtures.
- `lib/test/assert-test.f` — focused coverage for checked assertion primitives.
- `lib/test/suite.f` — private implementation body included by `lib/test.f`
  inside package `TEST`.
- `lib/test/suite-test.f` — focused package-scoped coverage for `TEST:*`
  setup/teardown hooks, groups, tests, stdin tests, filters, and argument feeds.
- `tools/ptx/cuda-launch.f`, `tools/ptx/softmax-launch.f`, and
  `tools/ptx/softmax-gradcheck.f` — Orin CUDA Driver proofs for launch,
  softmax, and finite-difference gradient checking.
- `tools/ptx/bandwidth.f` — Orin SAXPY bandwidth measurement for the Habu-PTX
  column.
- `maki/README.md` / `maki/STATUS.md` — Maki framework overview and current
  verification status outside the Habu trust root.
- `tools/srclist.f` — canonical source order.
- `tools/build-fixpoint.f` — checked self-rebuild fixpoint orchestration definitions.
- `tools/build-fixpoint-main.f` — CLI entrypoint for the self-rebuild fixpoint driver.
- `tools/build-fixpoint-test.f` — checked fixture coverage for the self-rebuild fixpoint driver.
- `tools/lint/json-writer.f` — compact JSON writer for native lint diagnostics.
- `tools/lint/source-lex.f` — checked vector-backed source lexer for native lints.
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
- `tools/diag-to-sarif.f` — converts diagnostic JSONL to SARIF for CI/review UIs.
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
- `tools/aot-call-report-lib.f` — reusable AOT call-stencil report scanner.
- `tools/aot-call-report.f` — CLI entrypoint for AOT call-stencil reports.
- `tools/aot-call-report-test.f` — checked fixture coverage for AOT call-stencil reports.
- `tools/bundle-lib-core.f` — reusable stdlib bundle construction core.
- `tools/bundle-lib.f` — CLI wrapper for stdlib bundle construction.
- `tools/bundle-lib-test-lib.f` — load-only stdlib bundle fixture library for resident runner tests.
- `tools/bundle-lib-test.f` — checked fixture coverage for the stdlib bundle tool.
- `tools/examples-test.f` — checked fixture coverage for stdlib examples.
- `tools/filemap-lint.f` — freshness lint for this file.
- `tools/repl-lint-core.f` — reusable scanner rejecting REPL-baked code that exits the interactive session.
- `tools/repl-lint.f` — CLI wrapper for REPL exit lint.
- `tools/repl-lint-test-lib.f` — load-only REPL exit lint fixture library for resident runner tests.
- `tools/repl-lint-test.f` — checked fixture coverage for REPL exit lint.
- `tools/trust-lint-core.f` — reusable `TRUSTED.md` drift scanner core.
- `tools/trust-lint.f` — CLI wrapper for `TRUSTED.md` drift lint.
- `tools/trust-lint-test.f` — checked fixture coverage for `TRUSTED.md` drift lint.
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
- `tools/string.f` — shared checked byte-string helper library.
- `lib/string-test.f` — focused coverage for checked string helpers.
- `lib/json-write.f` — checked emit-only JSON writer vocabulary for fixtures and native tools.
- `lib/json-write-test.f` — focused coverage for JSON writer escaping, structure, and errors.
- `lib/memory.f` — checked OS-backed byte buffer allocation helpers.
- `lib/memory-test.f` — focused coverage for memory allocation and 64K buffer spans.
- `lib/vector.f` — checked growable cell-vector helpers backed by OS memory.
- `lib/vector-test.f` — focused coverage for vector growth, bounds, typed pointer storage, and iteration.
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

## Tests And Benchmarks

- `test/checker-assert.f` — shared quiet checker-candidate assertion helper for
  negative checked-source tests.
- `test/gate-pool.f` — bounded checked process pool used by native gate runners.
- `test/gate-pool-test.f` — focused fork-backed pool worker coverage.
- `test/run.f` — native test suite entry run directly by `bin/hb`.
- `test/run-lib.f` — side-effect-free resident native test suite implementation.
- `test/run-support.f` — minimal scheduler support for starting external phases before resident setup.
- `test/run-resident.f` — late-loaded resident scheduler that forks phase workers without loading every phase library.
- `test/run-shared-stdlib.f` — parent-loaded stdlib setup inherited by forked stdlib workers.
- `test/run-worker.f` — fork-worker dispatcher that routes a resident phase to its owned support file.
- `test/run-worker-stdlib.f` — resident stdlib/check-cli/lint/tail phase support and dispatch.
- `test/run-worker-engine.f` — resident engine repair/fixture/runtime/validation phase support and dispatch.
- `test/run-worker-diag.f` — resident checker-diagnostics phase support and dispatch.
- `test/run-worker-dict.f` — resident dictionary/checker phase support and dispatch.
- `test/run-worker-debug.f` — resident prop/debug phase support and dispatch.
- `test/run-worker-aot.f` — resident AOT phase support and dispatch.
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
- `test/gate-dictionary.f` — thin entry wrapper for dictionary/checker contracts.
- `test/gate-dictionary-lib.f` — side-effect-free dictionary/checker contract definitions.
- `test/gate-debug.f` — thin entry wrapper for prop/debug checks.
- `test/gate-debug-lib.f` — side-effect-free prop/debug gate definitions.
- `test/gate-build-hbb.f` — in-process checked hb-build helpers for positive AOT gate coverage.
- `test/gate-aot-positive.f` — thin entry wrapper for AOT positive checks.
- `test/gate-aot-positive-lib.f` — side-effect-free AOT positive gate definitions.
- `test/gate-aot-negative.f` — thin entry wrapper for AOT rejection checks.
- `test/gate-aot-negative-lib.f` — side-effect-free AOT rejection gate definitions.
- `test/gate-runner-lib.f` — side-effect-free phase dispatch definitions for native test runners.
- `test/gate-runner-support.f` — side-effect-free support bundle for focused runner-entry invocations.
- `test/gate-runner-entry.f` — tiny CLI entry for focused native runner dispatch.
- `test/gate-stdlib-inline-lib.f` — in-process stdlib gate slice dispatcher for resident runner forks.
- `test/gate-stdlib-tool-base-ready.f` — resident-runner sentinel that marks the common stdlib tool base as already loaded.
- `test/gate-stdlib-lint-tools.f` — in-process lint-tools group body loaded after shared setup.
- `test/prop-test.f` — implemented property-based checker-soundness test (in-process via `evaluate`).
- `test/engine-suite.f` — native engine behavior suite.
