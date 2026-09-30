---
title: Certify engine source on the whitebox engine
status: closed
priority: 1
issue-type: task
created-at: "2026-09-30T18:50:15.059347+02:00"
closed-at: "2026-09-30T23:40:00.000000+02:00"
close-reason: "Premise false, measured by the Fable certification-host pass: certification replays engine source under mirror authority (verify-source.f:1161-1165, checker.f:1053-1055, 1171-1195), so the certified text re-interns its own private words (CHECKER-RECORD-SYM, checker.f:9425-9430) and the host contributes only its named surface and PES rows. On a whitebox engine with the 2,120 private symbols in [477, 7607) retired in process, prefix-src, stage2-src and stdin-src certify rc 0 (6052 certified, 0 rejected), a private control reference is E-UNDEFINED, and a bad prefix still rejects -2805 (probe retire-certify.f, log retire-certify-4.log). Certification stays on the build host on every path; build-fixpoint-source stays a product SUITE because it proves the product can certify and refresh."
---

Problem: `tools/build-fixpoint-source-test.f` (SUITE `build-fixpoint-source`, `test/gate-stdlib-cases.f:156`) and `tools/build-fixpoint.f:1248` BF-CERTIFY-CORE-ACT certify generated engine source on a TO-CORE-rewound product engine. That resolves the private helpers of the core prefix through the checker, which 974304d0 removes from the product engine.
Acceptance:
- `build-fixpoint-source` becomes a WHITEBOX-SUITE.
- `BF-CERTIFY-CORE-ACT` refuses a sealed product host by name before it rewinds, reading the image class.
- `docs/bootstrap.md` names the certification host.
Files: `test/gate-stdlib-cases.f`, `tools/build-fixpoint.f`, `docs/bootstrap.md`.
Verify: the suite green on the whitebox engine; the refusal pinned on the product engine; `test/run.f`.
Depends: none. Parallel with everything.
Parent: habu-ship-only-the-d7d38629. Design: the Fable surface design of 2026-09-30 (~/.cache/tmp/heron-arm64/design-surface.md); census: ~/.cache/tmp/heron-arm64/size-census/.
