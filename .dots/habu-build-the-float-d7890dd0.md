---
title: Build the float benchmark before the float compiler
status: active
priority: 2
issue-type: task
created-at: "\"2026-08-02T21:15:44.060674+02:00\""
---

Orchestrator order: the float corpus exists and is measured BEFORE any float compiler work, and it is comprehensive. Build tools/codegen-compare-corpus3.f + cases3 + baseline3 per the established two-corpus convention: the OLD column measured and committed now, every new-column row a gap naming 'floats' (extend the gap capability enum). Comprehensiveness is an argued property, stated in the corpus header the way the first corpus argues its coverage: the rows must span the float SHAPE space - (1) T-DIST2 (maki/array.f:32) two-pointer accumulation loop, (2) T-SUM plain accumulation, (3) T-SGD! in-place mutation loop (scale-and-subtract), (4) T-NORM2 accumulation + fsqrt, (5) T-REL-L2 two accumulators + division, (6) RELU-F scalar branch-on-float-compare (maki/fmath.f or autograd.f - read the real body), (7) a conversion row (int to float and back - survey what the engine's own words are), (8) a pure scalar arithmetic row (the float ADD3 analogue), and (9) a float comparison feeding a branch. First SURVEY the engine's float vocabulary from the source (f+ f- f* f/ fsqrt f< fmax? how do r-typed values live on the data stack - unboxed? NaN behavior on comparison?) and record the findings in the corpus header - the future compiler leaf must match these semantics exactly, so the benchmark is also the semantics contract. Pinned inputs must be shape-revealing including negative values, zero, and at least one input where naive reassociation would change the answer (float addition is not associative - the benchmark must pin the engine's evaluation order). Transcendental-calling activations (SIGMOID/EXP) are OUT (they call big words); note them for later. All rows execute in the old column with pinned outputs; committed baseline; CHECK-EXACT covers the new table's exact facts; both existing corpora byte-identical.

Claim: agent=fbenchlane workspace=.jj-ws/habu-build-the-float-d7890dd0
