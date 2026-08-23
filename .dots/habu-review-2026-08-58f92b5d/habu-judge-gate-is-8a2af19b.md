---
title: "judge gate is host-bound: clang column inside the compared bytes"
status: active
priority: 1
issue-type: task
created-at: "2026-08-22T22:38:25.891221+02:00"
---

Problem: tools/judge/report.f:318-325 TEXT$ renders the per-row clang REF-CELL and the REFERENCE-NOTE ('clang flags ...', 'reference object: 5124 bytes', or 'no clang column on this host') ABOVE MARK$; base.f:211-213 CHECKED$ is everything before the marker and check.f:85-86 byte-compares it; judge-test.f:60-63 asserts DIFF-AT -1 and test/gate-stdlib-cases.f:323-325 schedules it. The committed test/compiler/judge-baseline.txt:88-137 pins every clang cell and '5124/4324/530' bytes. docs/codegen-parity.md:19-20 and report.f:20-21 say the reference column is informational. On this Linux host (codegen-compare-cc.f:265-268: no reference column) the suite is red for no Habu change, and per docs/proofs.md a clang cell is not falsifiable by a Habu mutation. Acceptance: REF-CELL and REFERENCE-NOTE render below MARK$ (or the checked half is compared structurally on old/chain/ds-old/ds-new rows); judge --check and the suite green on a host without clang and on one with it; the baseline regenerated once with the reason. Files: tools/judge/report.f, base.f, check.f, judge-test.f, test/compiler/judge-baseline.txt, docs/codegen-parity.md. Verify: judge --check 46 rows on this host. Depends: none. Ownership: judge. Claim: agent=judge-host workspace=.jj-ws/habu-judge-gate-is-8a2af19b
