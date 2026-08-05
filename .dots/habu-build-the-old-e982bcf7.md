---
title: Build the old-vs-new codegen comparison harness
status: active
priority: 2
issue-type: task
created-at: "\"2026-07-30T17:16:38.683939+02:00\""
---

Full context: standing directive 2026-07-30 - the new native chain's acceptance is a MEASURED head-to-head against the old emitters, not structural completion. Build the harness now so the finish line exists before the chain does: given a set of real checked words (start with the engine's own small primitives and lib words the old emitter compiles today), compile each through the OLD path and (as chain links land) the NEW chain, then compare (1) correctness - execute both on shared input vectors, byte-compare stack/memory results; (2) generated-code size in bytes per word; (3) speed - cycle/nanosecond timing over repeated execution, same harness discipline as the deadlock-guard budgets (measured, named, no bare sleeps). Output one plain-English report table per word plus a summary. Until the new chain emits, the harness runs the old path alone and records the baseline table - that baseline IS the first deliverable and must be committed with the harness. Habu-only, run by bin/hb, scheduled in a suite. The harness must not reimplement either compiler - it drives the real entry points.

Claim: agent=cmpharness workspace=.jj-ws/habu-build-the-old-cmp

Progress 2026-07-30: the harness is built and the baseline table is committed at
test/compiler/codegen-compare-baseline.txt. It measures eleven pinned corpus
words (tools/codegen-compare-corpus.f) covering arithmetic, a branch, a locals
frame, both loop forms, recursion, cell memory, byte memory and a byte scan,
plus an empty call used to calibrate the timings. For each word it records the
compiled size in bytes read from the word's own dictionary record, the values
the word leaves on the stack on pinned inputs, and how long one call takes,
timed over repeated execution with a named, measured budget and no sleeps.
Running it recreates the table and compares it with the committed copy: sizes
and outputs exactly, timings against a tolerance measured under deliberate CPU
oversubscription. The corpus is compiled by the real engine and executed as
itself; nothing in the harness models a compiler. The new chain's column is
absent because the chain does not emit yet - the table's row grammar already
carries a path word so new rows can join the old ones without a format change.
The test runs in the stdlib gate as suite codegen-compare.
