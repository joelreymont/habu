---
title: Add checked MEM subspans and alignment
status: open
priority: 1
issue-type: task
created-at: "2026-07-13T16:44:21.424964+02:00"
blocks:
  - habu-add-unique-bounded-527e05ca
---

Problem: a whole-allocation byte span cannot safely express bounded slices, typed element alignment, or vector/native access preconditions. Fix: extend package MEM with checked subspan construction, offset+length refinement, alignment evidence, and typed byte/element views that preserve region, extent, access, persistence, and generation parameters. Acceptance: overflow, offset+len overflow, misalignment, wrong element size, cross-region evidence, and parent-generation mismatch reject or return typed diagnostics; valid nested subspans stay within the parent and preserve ownership. Files: lib/memory-region-subspan.f, focused test, docs/stdlib.md. Verify: red-first checker/runtime matrix, exact test load, typed-local diff lint, memory/linear suites, full native gate.
