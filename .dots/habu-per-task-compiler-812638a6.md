---
title: Per-task compiler context registries
status: open
priority: 2
issue-type: task
created-at: "2026-07-28T18:05:11.510513+02:00"
---

Full context: IR-CTX (src/compiler/ir/context.f) has an atomic generation counter, but the registry stack (DEPTH/GENS/BASES) is process-wide and assumes single-task compilation, like the shared SHA-256 state src/compiler/digest.f already documents. Before concurrent compilation lands, give each task its own registry (or guard entry with task ownership) and add a concurrency witness test in the style of test/compiler/ir-id-concurrency.f. Dependency: the concurrency model decision for the compiler pipeline.
