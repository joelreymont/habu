---
title: Index symbols with owned collision-safe lookup
status: closed
priority: 2
issue-type: task
created-at: "2026-09-12T18:11:06.102488+03:00"
closed-at: "2026-09-16T14:34:50.145893+03:00"
close-reason: "superseded by habu-campaign-c1-finish-1f129a00: Residue: the bucket index landed but the rebuilt full gate and the all-AOT speed pair have not certified it"
blocks:
  - habu-honour-the-committed-615f47a9
---

Plan: [PLAN.md](../../PLAN.md). Design reconciled 2026-09-13; replaces stale diagnosis/claim. Claim: unassigned.

Own ir/symbol.f private index/NEW/NEW-FROM/INTERN, build.f ownership plumbing and lifetime tests. Keep insertion-ordered rows/bytes authoritative. Context-owned mutable hash buckets yield candidate ordinals, then full length/byte comparison. Validate arena generation/state before index dereference; invalidate before teardown. Clone without mutable alias; reserve index/row/byte growth before publication; duplicate hits allocate nothing. Preserve owner/stale/frozen/capacity and failed-insertion semantics. Verify collisions, probe scaling, clone memory/setup, slot reuse/teardown. No arbitrary write API in append-only IR; all-AOT floor measured at campaign acceptance.

Verification: focused real-load cases above; rebuild and run `bin/hb --load test/run.f` for compiler/runtime integration. Speed acceptance uses the all-AOT campaign pair; functional/count evidence can be developed in parallel.

Integrated `41c7af94` after Cedar's independent source review and original-name
fresh checker-handover run. Symbol, arena and build cases pass on the candidate;
independent symbol cases pass with 774/1692/3197 successful bucket probes for
512/1024/2048 names. Includes collision reinterning, allocation-free hits,
capacity refusal before growth, clone independence, teardown and stale reuse.
The private index uses a generation-validated arena slot and an install-once
retirement observer. Full rebuilt gate and all-AOT speed measurement remain open.
