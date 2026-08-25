---
title: Bind performance evidence owners
status: open
priority: 1
issue-type: task
created-at: "2026-07-19T23:04:30.208840+02:00"
---

tools/kernel-perf-lint-core.f COVER-CHECK treats MEAS#>0 as generic evidence and immediately accepts every watched emitter. Measurement rows have a kernel name but no emitter identity, and the lint never relates that kernel to WATCH-PATHS. One unrelated measurement appended to perf-rows.tsv therefore satisfies changes to one or many other emitters; tests cover only one touched emitter and do not test cross-kernel/cross-emitter measurements. PERF:EMITTER? is not canonical either: broad `lib/ptx/cg*.f` and `tools/ptx/*-cg.f` predicates accept nonexistent near-matches such as lib/ptx/cgarbage.f and tools/ptx/not-an-emitter-cg.f, contradicting the exact-producer requirement and the closed dot's claim that unknown emitters reject.

Extend canonical measurement identity with the exact owning emitter and drive both diff watching and TSV decode from one exact package-owned emitter/producer inventory; no prefix/suffix heuristic. Require every touched emitter to have its own same-change measurement or newly versioned waiver; reject unrelated, duplicate, nonexistent, near-prefix/suffix, unknown, ambiguous, many-to-one, and one-row-for-two-touch evidence. Preserve historical measurements through an explicit versioned migration, not a permissive fallback. Add exact multi-emitter and cross-kernel mutation matrix, canonical TSV round trips, exhaustive inventory membership, near-match/nonexistent mutations, and prove every watched path maps to evidence before master can pass. Measure parser/JIT/DATA and lint time. Files: tools/ptx/perf-registry.f, perf rows/renderer/tests, tools/kernel-perf-lint-core.f/tests, docs/kernel-principles.md.
