---
title: Bounds-check public SPEC accessors
status: open
priority: 1
issue-type: task
created-at: "2026-07-19T20:56:48.587252+02:00"
---

maki/spec.f exposes raw-n indexed metadata without guards. SPEC-FREE@/SPEC-CT@ use i directly in fixed name/length arrays; SPEC-FAC-NAME@/RANK@ use f directly; SPEC-FAC-IDX@/GATHER@ adds unchecked k to the factor window and privately casts it to sp-fi; SPEC-FREE/CT-EXTENT@ inherit those reads. EQ-FROW@/FCOL@/ADJ@ take a typed eq-slot but index its fixed factor columns with unchecked k. Negative, count, count+1, and large/wrapping inputs therefore read outside the live rows or allocation and can return arbitrary spans, extents, numbers, or a forged eq-slot. Current tests cover only valid positions. Add one checked owner guard per domain before all address arithmetic: free/contract indices against their live counts, factor against SP-FAC-N, factor member against that factor rank/window, and equation factor against EQ-K@ and EQ-FCAP. Reject with a named SPEC bounds error before any read/cast. Add mutation/property tests for every public accessor at -1/count/count+1/large, zero-rank factors/equations, cross-factor window bleed, canaries around every table, and the invariant that any returned span lies wholly inside its live arena. Audit the remaining public spec table accessors for the same shape. Files: maki/spec.f, spec-test.f, spec-attention-test.f, error registry. Depends: none. Ownership: SPEC metadata read bounds only; no parser/derivation semantics or registry factoring.

Claim: agent=specbounds workspace=.jj-ws/fable-specbounds machine=spark (owns bounds-checking the public SPEC accessors)
