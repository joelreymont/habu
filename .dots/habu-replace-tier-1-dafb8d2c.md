---
title: Replace tier 1 and the tier-0 JIT with one-pass codegen
status: open
priority: 2
issue-type: task
created-at: "2026-10-22T12:43:01.623597+03:00"
---

Target: docs/architecture.md 'Platforms and code generation' (:210-235): one codegen per platform, one pass over the checked events (stack values in registers, constants left unemitted until needed, comparisons folded into branches, primitives inlined from the primitive table's bodies, Wasm structured control mapped directly); no IR and no optimizing pass unless a benchmark shows it pays. It replaces tier 1 (59,710 lines: token-tape re-read, IR module, passes, two verifiers, register allocation; 524 us for a trivial word, `tools/compile-floor.f` (trivial-t1)) and the ARM64 tier-0 JIT (src/habu/jit.f). Acceptance: one codegen per platform, tier 1 and jit.f deleted; `tools/compile-floor.f` re-measured for the new codegen; every waiting case below passes. Waiting cases (tier-1 reproducers, carried as this codegen's tests): habu-move-whole-values-6bcab743, habu-refuse-a-scanless-5c10d37f, habu-report-a-refused-de5d1404, habu-retire-the-arm64-20c7d262, and the closed habu-compile-an-empty-2d88de28, habu-compile-records-over-9e5f9ea6, habu-refuse-an-unsigned-ea48c18b, habu-refuse-a-second-382f76b5 (dot find <id>); habu-compile-a-does-4cd90ab9 and habu-admit-a-pkg-0239019d are fixed at tier 1 now and their tests carry over. Needs a Plan design before any lane. Depends: none. Claim: unassigned.
