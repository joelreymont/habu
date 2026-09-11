---
title: Make register allocation linear in body size
status: active
priority: 2
issue-type: task
created-at: "\"2026-09-11T16:38:06.263647+03:00\""
---

Problem: A64RA:ALLOCATE 39.2 s per load, vals^1.71 in the top decile; ANSWER-COUNT alone 7.1 s, ANSWER-RECORD 5.5 s, CHECK-GROUP 3.0 s; the 79 bodies over 256 values cost 25 s in allocation. The member-list change (cedar, parent 41df9051) removed global pairwise class scans for 7% on ANSWER-COUNT. Acceptance: allocation time fits a slope at most 1.1 against SSA values over the Tender per-definition set; every allocator negative kept; forced-AOT regalloc/loop/edge suites green; controlled pair. Files: src/compiler/native/regalloc.f. Verify: tender-perdef scaling fit; the allocator suites. Depends: none. Ownership: cedar. Claim: unassigned


Current ownership and handoff: Owner: cedar with /root/compiler_xhigh_review implementing the next regalloc.f-only optimization on 92ef13f0. Cross-reference habu-reduce-remaining-superlinear-9ece2395 for current measured history and diagnosis.
