---
title: Wire the new chain into the comparison harness
status: active
priority: 2
issue-type: task
created-at: "\"2026-07-31T18:22:53.909387+02:00\""
---

The native chain is complete for the straight-line subset (tape, elaborator NELAB, HIR, selector A64SEL, allocator A64RA + validator A64RAV, emitter A64EMIT - bytes proven by execution in test/compiler/native-emit.f). tools/codegen-compare.f holds the pinned 11-word corpus and the committed old-emitter baseline (bytes and cost per word, test/compiler/codegen-compare-baseline.txt). Wanted: a new-chain column - for every corpus word the straight-line subset can express, drive the real chain end to end (build HIR, select, allocate, accept, emit), record byte count and measured cost the same way the old column is measured, execute the emitted routine and compare its result against the old word's result on the same pinned inputs, and print old vs new side by side. A corpus word the subset cannot express yet is reported by the named capability it lacks (control flow, locals, calls), never silently skipped or approximated. This is the goal's finish line: fewer bytes, fewer nanoseconds, identical results, measured not claimed. Owners: CODEGEN-COMPARE plus a test in the harness's own suite.

Claim: agent=comparelane workspace=.jj-ws/habu-wire-the-new-8428fee8
