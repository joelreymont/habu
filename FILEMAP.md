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

- `tools/build.sh` — self-host rebuild/fixpoint.
- `tools/seed.sh` — install a trusted native seed, then rebuild current source.
- `tools/snap-hb.sh` — refreshes the checked `bin/hb` image.
- `tools/check.sh` — checked native engine wrapper.
- `tools/hb-build.sh` — standalone binary builder.
- `tools/srclist.f` — canonical source order.
- `tools/lint/json-writer.f` — compact JSON writer for native lint diagnostics.
- `tools/lint/source-lex.f` — shared source lexer for native lints.
- `tools/signature-lint.f` — strict typed-signature lint.
- `tools/signature-lint-test.sh` — fixture coverage for strict typed-signature lint.
- `tools/aot-lint.f` — stripped-AOT unsupported-word lint.
- `tools/aot-lint-test.sh` — fixture coverage for stripped-AOT source lint.
- `tools/diag-origin.f` — injects source-origin markers for checker JSON.
- `tools/diag-origin-test.sh` — fixture coverage for diagnostic origin markers.
- `tools/json-only.f` — keeps wrapper JSON mode machine-only on known diagnostics.
- `tools/json-only-test.f` — checked fixture coverage for JSON diagnostic filtering.
- `tools/gate-json-assert.f` — native JSON assertions for the default gate.
- `tools/host-lint.f` — rejects retired host-script workflow tokens.
- `tools/check-all-errors.f` — batches checker diagnostics by top-level definition.
- `tools/check-all-errors-test.sh` — fixture coverage for all-errors checking.
- `tools/diag-to-sarif.f` — converts diagnostic JSONL to SARIF for CI/review UIs.
- `tools/public-signatures.f` — emits typed public-word manifests for agents.
- `tools/aot-call-report.f` — measures patched AOT call-stencil padding.
- `tools/filemap-lint.f` — freshness lint for this file.
- `tools/trust-lint.f` — `TRUSTED.md` drift lint.
- `tools/trust-lint-test.sh` — fixture coverage for `TRUSTED.md` drift lint.
- `tools/stale-status-lint.f` — stale status/count lint.
- `tools/parallel-agent-lint.f` — freshness lint for the parallel-agent protocol.
- `tools/string.f` — shared checked byte-string helper library.
- `lib/string-test.f` — focused coverage for checked string helpers.
- `tools/date.f` — shared checked UTC Gregorian date parsing, formatting, and timestamp helpers.
- `tools/date-test.f` — focused coverage for shared date helpers.

## Tests And Benchmarks

- `test/run.sh` — default native gate.
- `test/prop-test.f` — self-hosted property-based checker-soundness test (in-process via `evaluate`).
- `test/engine-suite.f` — native engine behavior suite.
- `bench/llm/tasks.tsv` — LLM benchmark task set.
- `bench/llm/solutions.f` — reference benchmark solutions.
- `bench/llm/validate-results.f` — native reference/attempt metric validator and summarizer.
- `bench/llm/validate-results-test.sh` — fixture coverage for metric validation.
- `bench/llm/run-attempts.sh` — turns per-task candidate/repair files into validator-schema JSONL.
- `bench/llm/attempt-runner-test.sh` — fixture coverage for schema attempt generation.
- `bench/llm/habu-array-lib.f` — checked array helper vocabulary for library-assisted Habu benchmark arm.
- `bench/llm/habu-array-lib-test.f` — focused coverage for the benchmark array helper library.
- `bench/llm/habu-preamble-lib.txt` — LLM prompt preamble for library-assisted Habu benchmark arm.
- `bench/llm/run.sh` — benchmark certification runner.
- `bench/llm/perf.sh` — LLM feedback-loop performance benchmark runner.
