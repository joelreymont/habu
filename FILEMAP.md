# FILEMAP.md

Fast orientation for coding agents when code search is unavailable or expensive.
`tools/filemap-lint.py` keeps these paths live and checks that required entry
points stay listed.

## Agent Context

- `AGENTS.md` — repository conventions, workflow, and gate.
- `LLM.md` — operating protocol for LLM-written Forth.
- `LESSONS.md` — running project memory and recent implementation findings.
- `STATUS.md` — current verification status and known gaps.
- `TRUSTED.md` — audited `TRUST` escape-hatch manifest.
- `docs/forth.md` — blocking Forth style rules.

## Core Checker

- `src/core/util.f` — shared subset helpers.
- `src/core/checker.f` — native stack-effect checker and verifier.
- `src/core/render.f` — human/JSON diagnostics and signature recording.
- `bootstrap/src/checker.fs` — gforth-hosted reference checker.
- `bootstrap/src/sigparse.fs` — reference signature parser.
- `bootstrap/src/colon.fs` — gforth checked-colon integration.

## Native Engine And Builders

- `src/habu/habu1.f` — primitive engine, dictionary, parser, and prim registry.
- `src/habu/habu2.f` — compiler/control-flow layer.
- `src/habu/jit.f` — register JIT helpers.
- `src/habu/regalloc.f` — virtual stack register allocator.
- `src/habu/aot.f` — stripped AOT linker driver.
- `src/habu/build.f` — `hb-build --repl` bundle driver.
- `src/habu/snap.f` — warm snapshot image writer.
- `src/habu/hbi.f` — stdin/interactive engine builder.

## Mach-O And Signing

- `src/os/macos/macho.f` — Mach-O image construction.
- `src/os/macos/sign2.f` — embedded ad-hoc signature writer.
- `docs/macho.md` — Mach-O layout notes.

## Tools And Gates

- `tools/build.sh` — self-host rebuild/fixpoint.
- `tools/snap-hb.sh` — warm checked snapshot builder.
- `tools/check.sh` — checked native engine wrapper.
- `tools/hb-build.sh` — standalone binary builder.
- `tools/oracle.sh` — gforth differential gate.
- `tools/srclist.sh` — canonical source order.
- `tools/signature-lint.py` — strict typed-signature lint.
- `tools/aot-lint.py` — stripped-AOT unsupported-word lint.
- `tools/forth_lex.py` — shared lexer for source lints.
- `tools/diag-origin.py` — injects source-origin markers for checker JSON.
- `tools/filemap-lint.py` — freshness lint for this file.
- `tools/trust-lint.py` — `TRUSTED.md` drift lint.
- `tools/stale-status-lint.py` — stale status/count lint.

## Tests And Benchmarks

- `test/run.sh` — default native gate.
- `test/all.fs` — gforth-hosted suite entry.
- `test/t-sh-jdiag.fs` — JSON diagnostic regression tests.
- `test/t-sh-verify.fs` — native `CHECK!` verification tests.
- `test/hb-suite.f` — native engine behavior suite.
- `bench/llm/tasks.tsv` — LLM benchmark task set.
- `bench/llm/solutions.f` — reference benchmark solutions.
- `bench/llm/run.sh` — benchmark certification runner.
