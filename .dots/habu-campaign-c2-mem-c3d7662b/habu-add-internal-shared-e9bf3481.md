---
title: Add internal shared view effects
status: closed
priority: 1
issue-type: task
created-at: "\"\\\"2026-09-30T09:09:55.507799+02:00\\\"\""
closed-at: "2026-09-30T12:04:55.827015+02:00"
close-reason: Implemented internal read-view scope/dependency effects, invariant two-cell transport, generic storage restrictions and portable graph metadata. Independent Astra source and merge review PASS. Private candidate c3038491 on reviewed source a3339471 passed focused load/image/pretrust/census/engine/type/diagnostics and full native suite 500/500, process exit 0. Five-generation chain has zero-byte differences for generations 2/3, 3/4 and 4/5. Proofs retained at /Users/joel/.cache/tmp/habu-c2-merged. C2 remains private; public constructors, lexical owner binders, loans and cleanup are subsequent campaign work.
---

Implement the C2 checker foundation from docs/ownership-model.md: a fixed two-cell read<P,L,T> layout with scope-kind parameters and structural dependency propagation through effects, records, sums and quotations. No public constructor or safe-view API yet. Checked ordinary helpers preserve input scopes; output-only or fresh-region scope forgery, scoped values in untracked/raw/typed storage, cast erasure and mutable-view admission reject. Write real --load positive and rejected source fixtures before checker changes, then focused E2E, full native suite and affected image/convergence checks. Keep the public surface internal until binder, loans, runtime cleanup and capture fences land.
