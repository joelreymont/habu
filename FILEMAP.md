# FILEMAP.md

Fast orientation for coding agents when code search is unavailable or expensive.
`tools/filemap-lint.f` keeps these paths live and checks that required entry
points stay listed.

## Agent Context

- `AGENTS.md` — repository conventions, workflow, and gate.
- `LLM.md` — operating protocol for LLM-written Forth.
- `LESSONS.md` — running project memory and recent implementation findings.
- `STATUS.md` — current verification status and known gaps.
- `TRUSTED.md` — audited `TRUST` escape-hatch manifest.
- `PROP-TESTING.md` — property-based checker-soundness design (generate→check→run→compare).
- `docs/forth.md` — blocking Forth style rules.
- `docs/llm-stdlib-cookbook.md` — prompt-sized checked stdlib examples for LLM-generated Habu.
- `docs/parallel-agents.md` — map-reduce protocol for parallel dot execution.
- `docs/seed.md` — native seed trust root and no-binary recovery.

## Core Checker

- `src/core/util.f` — shared subset helpers.
- `src/core/checker.f` — native stack-effect checker and verifier.
- `src/core/render.f` — human/JSON diagnostics and signature recording.
- `src/core/roles.f` — audited nominal scalar role conversion words.
- `src/core/sha256.f` — standalone SHA-256, streaming file digest, and hex helpers.

## Native Engine And Builders

- `src/habu/habu1.f` — primitive engine, dictionary, parser, and prim registry.
- `src/habu/habu2.f` — compiler/control-flow layer.
- `src/habu/jit.f` — register JIT helpers.
- `src/habu/regalloc.f` — virtual stack register allocator.
- `src/habu/aot.f` — stripped AOT linker driver.
- `src/habu/build.f` — `hb-build --repl` bundle driver.
- `src/habu/snap.f` — checked image writer for `bin/hb`.
- `src/habu/stdin.f` — internal stdin/interactive engine builder.

## Mach-O And Signing

- `src/os/macos/macho.f` — Mach-O image construction.
- `src/os/macos/sign2.f` — embedded ad-hoc signature writer.
- `docs/macho.md` — Mach-O layout notes.

## Tools And Gates

- `tools/seed.f` — checked native seed installer, SHA verifier, smoke test, and fixpoint rebuild driver.
- `tools/seed-main.f` — CLI entrypoint for checked native seed recovery.
- `tools/seed-test.f` — focused coverage for seed SHA, install, signing, and smoke helpers.
- `tools/build-fixpoint.f` — checked native stage/stdin/snapshot build driver.
- `tools/check.f` — Habu-native checked engine runner.
- `tools/check-test.f` — checked fixture coverage for the native check runner.
- `tools/sha256-file-test.f` — checked fixture coverage for streaming SHA-256 helpers.
- `tools/hb-cli-contracts-test.f` — checked coverage for `hb` startup and stdin-data contracts.
- `tools/hb-baseline-contracts-test.f` — checked public `bin/hb` baseline contract fixture.
- `tools/hb-build-lib.f` — checked native AOT/REPL build CLI library.
- `tools/hb-build.f` — Habu entrypoint for native AOT/REPL builds.
- `tools/hb-build-test.f` — checked fixture coverage for native AOT/REPL builds.
- `tools/imgdump.f` — native image dictionary dump and compare tool.
- `tools/imgdump-test.f` — checked fixture coverage for image dump compare mode.
- `tools/srclist.f` — canonical source order.
- `tools/build-fixpoint.f` — checked self-rebuild fixpoint orchestration definitions.
- `tools/build-fixpoint-main.f` — CLI entrypoint for the self-rebuild fixpoint driver.
- `tools/build-fixpoint-test.f` — checked fixture coverage for the self-rebuild fixpoint driver.
- `tools/lint/json-writer.f` — compact JSON writer for native lint diagnostics.
- `tools/lint/source-lex.f` — shared dynamic source lexer for native lints.
- `tools/signature-lint.f` — strict typed-signature lint.
- `tools/signature-lint-test.f` — checked fixture coverage for strict typed-signature lint.
- `tools/aot-lint.f` — stripped-AOT unsupported-word lint.
- `tools/aot-lint-test.f` — checked fixture coverage for stripped-AOT source lint.
- `tools/diag-origin.f` — injects source-origin markers for checker JSON.
- `tools/diag-origin-test.f` — checked fixture coverage for diagnostic origin markers.
- `tools/json-only.f` — keeps wrapper JSON mode machine-only on known diagnostics.
- `tools/json-only-test.f` — checked fixture coverage for JSON diagnostic filtering.
- `tools/gate-json-assert.f` — native JSON assertions for the default gate.
- `tools/repair-schema-doc-test.f` — checked fixture coverage for repair diagnostic schema docs.
- `tools/repair-packet-test.f` — checked fixture coverage for repair packet generation.
- `tools/check-repair-hints-test.f` — checked fixture coverage for repair-class hints.
- `tools/host-lint.f` — rejects retired host-script workflow tokens.
- `tools/check-all-errors.f` — batches checker diagnostics by top-level definition.
- `tools/check-all-errors-test.f` — checked fixture coverage for all-errors checking.
- `tools/checked-boundary-lint-test.f` — checked fixture coverage for unchecked-boundary lint.
- `tools/diag-to-sarif.f` — converts diagnostic JSONL to SARIF for CI/review UIs.
- `tools/public-signatures.f` — emits typed public-word manifests for agents.
- `tools/public-signatures-test.f` — checked fixture coverage for public-signature manifests.
- `tools/stdlib-manifest-test.f` — validates `lib/std.manifest`, stdlib docs, and source-backed signatures.
- `tools/aot-call-report.f` — measures patched AOT call-stencil padding.
- `tools/aot-call-report-test.f` — checked fixture coverage for AOT call-stencil reports.
- `tools/bundle-lib-test.f` — checked fixture coverage for the stdlib bundle tool.
- `tools/examples-test.f` — checked fixture coverage for stdlib examples.
- `tools/filemap-lint.f` — freshness lint for this file.
- `tools/repl-lint.f` — rejects REPL-baked code that exits the interactive session.
- `tools/repl-lint-test.f` — checked fixture coverage for REPL exit lint.
- `tools/trust-lint.f` — `TRUSTED.md` drift lint.
- `tools/trust-lint-test.f` — checked fixture coverage for `TRUSTED.md` drift lint.
- `tools/host-lint-test.f` — focused coverage for host-script lint policy helpers.
- `tools/stale-status-lint.f` — stale status/count lint.
- `tools/stale-status-lint-test.f` — checked fixture coverage for stale status/count lint.
- `tools/parallel-agent-lint.f` — freshness lint for the parallel-agent protocol.
- `tools/string.f` — shared checked byte-string helper library.
- `lib/string-test.f` — focused coverage for checked string helpers.
- `lib/json-write.f` — checked emit-only JSON writer vocabulary for fixtures and native tools.
- `lib/json-write-test.f` — focused coverage for JSON writer escaping, structure, and errors.
- `lib/memory.f` — checked OS-backed byte buffer allocation helpers.
- `lib/memory-test.f` — focused coverage for memory allocation and 64K buffer spans.
- `lib/test-runner.f` — checked gate runner foundation for temp roots, captures, and aggregate failures.
- `lib/test-runner-test.f` — focused coverage for gate runner process, timeout, and failure aggregation helpers.
- `tools/date.f` — shared checked UTC Gregorian date parsing, formatting, and timestamp helpers.
- `tools/date-test.f` — focused coverage for shared date helpers.
- `lib/process-env.f` — checked child envp builder and PATH lookup helpers.
- `lib/process-env-test.f` — focused coverage for child envp and executable lookup.
- `lib/process-cwd.f` — checked child cwd process helpers layered on prepared argv/envp.
- `lib/process-cwd-test.f` — focused coverage for child cwd spawn, capture, cleanup, and validation.
- `lib/source.f` — checked source materialization and source-list transforms.
- `lib/source-test.f` — focused coverage for source materialization helpers.
- `test/process-env-child.f` — child fixture used by process-env tests.

## Tests And Benchmarks

- `test/run.f` — default native gate.
- `test/prop-test.f` — self-hosted property-based checker-soundness test (in-process via `evaluate`).
- `test/engine-suite.f` — native engine behavior suite.
- `bench/llm/tasks.tsv` — LLM benchmark task set.
- `bench/llm/models.tsv` — model registry for live benchmark sweeps.
- `bench/llm/manifest-audit.f` — checked required-row audit for expanded benchmark tasks.
- `bench/llm/manifest-audit-main.f` — CLI entry point for the benchmark manifest audit.
- `bench/llm/manifest-audit-test.f` — focused coverage for benchmark manifest audit checks.
- `bench/llm/model.f` — checked model registry scanner and selected-model metadata buffers.
- `bench/llm/model-test.f` — focused coverage for model registry parsing and validation.
- `bench/llm/codex-home.f` — checked Codex benchmark home isolation and config symlink setup.
- `bench/llm/codex-home-test.f` — focused coverage for Codex home isolation.
- `bench/llm/model-run.f` — native model command runner for live benchmark drivers.
- `bench/llm/model-run-test.f` — focused coverage for native model argv template expansion.
- `bench/llm/parse-resp-lib.f` — loadable model response parser library for live benchmark drivers.
- `bench/llm/parse-resp.f` — CLI wrapper for the model response parser.
- `bench/llm/vectors.f` — checked benchmark vector parser and Habu snippet emitter.
- `bench/llm/vectors-test.f` — focused coverage for vector parsing and emitted snippets.
- `bench/llm/foreign-vectors.f` — checked JS/Python/TypeScript/Rust vector and runtime snippet emitters.
- `bench/llm/foreign-vectors-test.f` — exact fixture coverage for foreign vector and runtime emitters.
- `bench/llm/forth-task-lines-lib.f` — checked harness=forth task-row scanner/emitter for live benchmark drivers.
- `bench/llm/forth-task-lines.f` — CLI wrapper for harness=forth task-row emission.
- `bench/llm/forth-task-lines-test.f` — focused coverage for task-row filtering and file output.
- `bench/llm/forth-candidate.f` — checked Forth candidate extractor, definition metadata scanner, and forbidden boundary token guard.
- `bench/llm/forth-candidate-test.f` — focused coverage for Forth candidate extraction, name/signature scanning, completion, and trusted-boundary rejection.
- `bench/llm/forth-bundle.f` — checked Forth task bundle builder that sizes a bundle, replaces one target with a candidate, and appends benchmark tests.
- `bench/llm/forth-bundle-test.f` — focused coverage for bundle sizing, replacement, missing files, duplicate rows, extra references, schema errors, and capacity failures.
- `bench/llm/large-buffer-bundle-test.f` — regression that composes the source lexer, task-row scanner, reference extractor, and many simultaneously live 64K buffer spans in one checked load.
- `bench/llm/drive-forth-lib.f` — native checked Habu live driver for harness=forth benchmark rows, including candidate extraction, dynamic bundle allocation, checker/test execution, and feedback modes.
- `bench/llm/drive-forth.f` — CLI wrapper for the native Forth live benchmark driver.
- `bench/llm/drive-forth-test.f` — focused coverage for native Forth driver pass, fail, checker reject, forbidden-boundary reject, row artifacts, and feedback-mode arms.
- `bench/llm/diagnostic-stats.f` — checked diagnostic field, repair-class event, and statistics reducer for benchmark rows.
- `bench/llm/diagnostic-json-check-stub.f` — checker-only JSON parser contract for the diagnostic stats reducer.
- `bench/llm/diagnostic-stats-check-test.f` — checker fixture for diagnostic stats aggregation without loading the JSON recovery boundary.
- `bench/llm/diagnostic-stats-test.f` — focused coverage for diagnostic field booleans, repair-class event extraction, and aggregation.
- `bench/llm/artifacts.f` — checked replay artifact path, SHA-256, and JSON field helpers.
- `bench/llm/artifacts-test.f` — focused coverage for replay artifact hashing and JSON fields.
- `bench/llm/live-row.f` — checked schema-v2 live benchmark row emitter with replay artifacts.
- `bench/llm/live-row-test.f` — focused coverage for native live benchmark row emission.
- `bench/llm/fixture-text.f` — checked source-literal and TSV text fixture builder words.
- `bench/llm/fixture-text-test.f` — focused checks for fixture text builder syntax words.
- `bench/llm/driver-test-helpers.f` — shared checked fixture/source helpers for live driver tests.
- `bench/llm/driver-token-helpers.f` — checked exact-token source guards for large live drivers that need prefix-safe checks.
- `bench/llm/driver-fixture-helpers.f` — checked DSL for generated benchmark fixture vocabularies and test bundles.
- `bench/llm/drive-stdlib-lib.f` — checked shared stdlib driver base with buffers, artifacts, capture, candidate extraction, tests, and row setup.
- `bench/llm/drive-stdlib-live.f` — live model-run boundary for the stdlib stack benchmark driver.
- `bench/llm/drive-stdlib.f` — CLI wrapper for the stdlib stack benchmark driver.
- `bench/llm/drive-stdlib-test.f` — focused coverage for stdlib stack driver acceptance and source-use guards.
- `bench/llm/drive-regex-negative-lib.f` — native stdlib regex negative benchmark driver with expected-code scoring.
- `bench/llm/drive-regex-negative.f` — CLI wrapper for the regex negative benchmark driver.
- `bench/llm/drive-regex-negative-test.f` — focused coverage for regex negative expected-code, wrong-code, and silent-success outcomes.
- `bench/llm/drive-file-lib.f` — native stdlib file benchmark driver using generated `FS-FIX-*` fixture words.
- `bench/llm/drive-file.f` — CLI wrapper for the stdlib file benchmark driver.
- `bench/llm/drive-file-test.f` — focused coverage for stdlib file driver read/write/append and capacity-negative fixtures.
- `bench/llm/drive-process-lib.f` — native stdlib process benchmark driver using executable generated `PROC-FIX-*` Habu fixtures.
- `bench/llm/drive-process.f` — CLI wrapper for the stdlib process benchmark driver.
- `bench/llm/drive-process-test.f` — focused coverage for stdlib process driver rc/capture/nonzero and timeout/truncation fixtures.
- `bench/llm/drive-property-lib.f` — native stdlib property benchmark driver with exact-token stdlib-use guards.
- `bench/llm/drive-property.f` — CLI wrapper for the stdlib property benchmark driver.
- `bench/llm/drive-property-test.f` — focused coverage for property default/random/generator/shrink and bad-seed fixtures.
- `bench/llm/drive-build-lib.f` — native stdlib build benchmark driver using Habu-generated source/build-script fixtures.
- `bench/llm/drive-build.f` — CLI wrapper for the stdlib build benchmark driver.
- `bench/llm/drive-build-test.f` — focused coverage for build check/artifact/step-status/run/missing-artifact fixtures.
- `bench/llm/drive-aot-lib.f` — native stripped-AOT benchmark driver using `tools/hb-build.f`.
- `bench/llm/drive-aot.f` — CLI wrapper for the stripped-AOT benchmark driver.
- `bench/llm/drive-aot-test.f` — focused coverage for AOT positive build/run and unsupported-token rejection fixtures.
- `bench/llm/drive-array-habu-lib.f` — native Habu array benchmark driver library with array-arm bundling, skeleton wrapping, repair packets, and vector grading.
- `bench/llm/drive-array-habu.f` — CLI wrapper for the native Habu array benchmark driver.
- `bench/llm/drive-array-habu-test.f` — focused coverage for native Habu array driver pass, fail, reject, and repair-packet outcomes.
- `bench/llm/drive-array-habu-repair-test.f` — focused live-model coverage for native Habu array repair-loop success and row accounting.
- `bench/llm/drive-foreign-lib.f` — native foreign-language benchmark driver core for JavaScript, Python, Rust, and TypeScript function arms.
- `bench/llm/drive-foreign-live.f` — live model-run boundary for foreign-language benchmark drivers.
- `bench/llm/drive-foreign-check-test.f` — source-list fixture proving the foreign driver core checks without model-run or response-parser boundaries.
- `bench/llm/drive-js.f` — CLI wrapper for the native JavaScript array benchmark driver.
- `bench/llm/drive-js-test.f` — focused coverage for native JavaScript pass, fail, timeout, runtime, and repair-loop outcomes.
- `bench/llm/drive-python.f` — CLI wrapper for the native Python array benchmark driver.
- `bench/llm/drive-python-test.f` — focused coverage for native Python pass, fail, timeout, runtime, and repair-loop outcomes.
- `bench/llm/drive-rust.f` — CLI wrapper for the native Rust array benchmark driver.
- `bench/llm/drive-rust-test.f` — focused coverage for native Rust pass, reject, fail, timeout, runtime, and repair-loop outcomes.
- `bench/llm/drive-ts.f` — CLI wrapper for the native TypeScript array benchmark driver.
- `bench/llm/drive-ts-test.f` — focused coverage for native TypeScript pass, fail, timeout, runtime, and repair-loop outcomes.
- `bench/llm/negative-score.f` — checked scorer for negative benchmark expected codes/tokens/classes.
- `bench/llm/negative-score-test.f` — focused coverage for negative-harness scoring outcomes.
- `bench/llm/run-expanded-bench.f` — native expanded live benchmark runner and arm dispatcher.
- `bench/llm/run-expanded-bench-test.f` — focused coverage for expanded-run AOT dispatch and report validation.
- `bench/llm/report-test.f` — focused checked coverage for legacy report summary rows and arm/category rendering.
- `bench/llm/expanded-report.f` — expanded live benchmark Markdown report with validator and latency sections.
- `bench/llm/expanded-report-test.f` — focused coverage for expanded report perf-latency rendering.
- `bench/llm/grade.f` — native isolated benchmark grader for pass/fail/reject/trap/timeout outcomes.
- `bench/llm/grade-test.f` — focused coverage for native benchmark grader outcome classification.
- `bench/llm/solutions.f` — reference benchmark solutions.
- `bench/llm/validate-results.f` — native reference/attempt metric validator and summarizer.
- `bench/llm/validate-results-test.f` — checked positive and rejection fixture coverage for metric validation.
- `bench/llm/perf-lib.f` — native LLM feedback-loop performance timing harness.
- `bench/llm/perf.f` — CLI wrapper for native LLM feedback-loop performance timing.
- `bench/llm/perf-test.f` — focused coverage for native perf JSON/text/options.
- `bench/llm/run-attempts-lib.f` — checked attempt-runner helpers for deterministic candidate round enumeration, task looping, test-bundle assembly, checker/test execution, per-attempt metric state, and schema-1 row emission.
- `bench/llm/run-attempts-test.f` — focused coverage for candidate enumeration, task looping, bundle assembly, checker/test execution, attempt metrics, and parsed row output.
- `bench/llm/run-attempts-check-test.f` — checker-safe smoke coverage for attempt runner helper and row-emitter effects with JSON parser stubs.
- `bench/llm/run-attempts.f` — checked CLI that turns per-task candidate/repair files into validator-schema JSONL and validates the result.
- `bench/llm/run-attempts-cli-test.f` — end-to-end checked fixture for the attempt runner CLI over the real forth task corpus.
- `bench/llm/run-attempts-cli-check-test.f` — checker-safe smoke coverage for attempt runner CLI helpers.
- `bench/llm/habu-array-lib.f` — checked array helper vocabulary for library-assisted Habu benchmark arm.
- `bench/llm/habu-array-lib-test.f` — focused coverage for the benchmark array helper library.
- `bench/llm/habu-preamble-lib.txt` — LLM prompt preamble for library-assisted Habu benchmark arm.
- `bench/llm/run.sh` — benchmark certification runner.
