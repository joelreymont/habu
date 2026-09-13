---
title: "Digest a definition's source once per pass"
status: open
priority: 2
issue-type: task
created-at: "2026-09-11T17:01:31.578224+03:00"
blocks:
  - habu-compile-the-tender-8385810f
---

Deferred by [PLAN.md](../PLAN.md),2026-09-13. Historical contribution75ms/179s; defer until whole-build target is measured. No current claim.

Problem: each SOURCE! verify (src/compiler/native/select.f:2880, spill.f:829, combine.f:776, loop.f:1325) hashes the definition text, compares, then calls IR-BUILD:ADD-SOURCE which hashes the identical bytes again: 4,778 redundant SHA-256 passes per Tender load, 75 ms of 179 s (audit 2026-09-11, .jj-ws/rowan-hash/tmp/audit). Acceptance: a public IR-SOURCE:REGISTER-CK ( ctx arena key ptr u8 n digest -- id ) that digests once, compares, and only then reserves and appends (refusal before any row write, as ROW-ADD does), an ADD-SOURCE-CK wrapper in build.f, the four verify sites using it; registration digests byte-identical (same SHA-256, same bytes, same point); counter shows 12,645 to 7,867 calls per load. Files: src/compiler/ir/source.f, src/compiler/ir/build.f, src/compiler/native/select.f, spill.f, combine.f, loop.f. Verify: the digest counter harness over the frozen Tender snapshot; ir-source and native suites. Depends: habu-read-an-ir-516b2416 (source.f is mid-migration). Ownership: rowan. Claim: unassigned.
