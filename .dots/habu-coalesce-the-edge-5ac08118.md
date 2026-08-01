---
title: Coalesce the edge copies
status: active
priority: 2
issue-type: task
created-at: "\"2026-08-01T17:10:48.931523+02:00\""
---

Phase 2, third quality pass. The selector inserts one a64.mov per value crossing an argument-carrying edge (the interference-free-by-construction design from the multi-block allocation leaf), and the allocator gives the copy's ends separate registers even when they could share, so loop latches carry movs that move a value to where it already is or could have been. Wanted: coalescing by allocation PREFERENCE - when a copy's source and destination classes do not interfere, the union-find merges them so both ends get one register, and the emitter then elides a mov whose source and destination registers are equal (a self-move moves nothing - the elision is register equality at emission, the same single-rule discipline as branch elision: one word, layout and writer both ask it, cursor check already in place catches drift). The allocator half must keep the validator honest: A64RAV re-derives the merged classes independently and the edge clause still holds. Refusals unchanged - a merge that would create interference simply does not happen (fall back to the copy), never a wrong allocation. Acceptance: full-table before/after with the drift-corrected control-row methodology; expect loop rows (SUM-TO, COUNT-DOWN, BYTE-SUM, BYTE-FIND, FACT latch copies) to lose bytes and cost; results identical everywhere; revert if nothing moves. Mutations: merge despite interference (must be refused or fall back, proven by a fixture with genuinely interfering ends), self-move elided when registers differ (dies by execution), layout/writer count drift (E-A64EMIT-LAYOUT).

Claim: agent=coalane workspace=.jj-ws/habu-coalesce-the-edge-5ac08118
