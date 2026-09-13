---
title: Finish tier dispatch, checker owners and retained-code provenance
status: active
priority: 1
issue-type: task
created-at: "\"\\\"2026-09-11T16:07:57.854352+03:00\\\"\""
blocks:
  - habu-keep-a-row-f2c4f3d4
---

Plan: [PLAN.md](../../PLAN.md). Design reconciled 2026-09-13; replaces stale diagnosis/claim. Claim: active; Astra owner-interface lane first, Cedar review/integration. Owner-only extraction from pendinge0ca8e73/a91c8580/43bb0148 and selected670eda3c tests preserves provider799f713b. Excludes early native-build checker-owner/PRE-RECORD-HOST load, provenance band changes, routing and inliner deletion. This partial interface work does not complete the full tier contract.

Own rowan-tier91fff115 dispatch/provenance in habu2.f, checker owner-record declarations and src/compiler/native owner/tape/front-end consumers; exclude call-row/payload fixes, build routing, prefix tables and messages. Reuse pending stack with definition-tier latch. Reject JIT compiler entry/tier0 requests during AOT before emission; NCOMP absence/failure preserves original error without fallback. Provenance covers hidden/re-exposed spans, aliases, DOES, stored quotations/direct calls; record count alone fails. Verify tier parity/owner isolation, span bounds, AOT saves and retained-JIT negatives. Native execution and emitting REPL support remain legal.

Verification: focused real-load cases above; rebuild and run `bin/hb --load test/run.f` for compiler/runtime integration. Speed acceptance uses the all-AOT campaign pair; functional/count evidence can be developed in parallel.

Source-owner slice integrated as09bc4b7e after independent Astra review and focused native-window/tape/tier tests on c3d0888c. Its54-cell owner record, callback arities, absent-owner/missing-field refusals and CWIN transfer were verified. This closes only the source-owner portion: retained-JIT provenance remains assigned to the Astra tier lane; routing/layout remain separate leaves.

Typed indirect dispatch is integrated at `a5b1956b` after independent review and
a fresh tier1 checker/adapter load, exact `window: 0` with no diagnostics.
All 38 wrappers now expose callable layouts through 15 narrow typed views.
The full tier1 source load next fails at `A64IR-OPCODE:TAG` (-8650). Reduced case:
the source checker accepts a fresh sum and resolves family36; retained NFAM
reports it absent. NFAM's static TFL/TFAM bindings must follow that same owner.
The active owner lane is adding canonical record ABI fields and a paired native
compiler bridge; merely loading new wrappers cannot repair old native call sites.
Independent max Astra audit found no further live checker-state binding outside
NFAM. Provenance runtime verification still awaits the repaired compiler.
