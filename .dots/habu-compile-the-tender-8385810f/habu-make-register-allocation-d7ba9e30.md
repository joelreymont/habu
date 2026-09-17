---
title: Make register allocation linear in body size
status: closed
priority: 2
issue-type: task
created-at: "\\\"2026-09-11T16:38:06.263647+03:00\\\""
closed-at: "2026-09-12T17:23:38.393634+03:00"
close-reason: "landed 7f49cf17 (four commits on 62f5dbb3): tools/compile-scaling.f yardstick with the allocator's stopwatch, then class starts bucketed by position, free-register counts maintained per file, the module's calls indexed once; every allocator negative and the forced-AOT regalloc/loop/edge suites green; quiet-box pair (load 1.05-2.08) chain slope 1.15 -> 0.97 and locals 1.12 -> 0.95, confirmed on the root engine at load 0.47 (locals 0.95, ratchet -- 1.1 rc 0); Tender source load under the forced optimizing tier 153.3 s -> 131.6 s; the unmeasured POS-BLOCK bisection was dropped; reviewed by hazel; follow-ups habu-give-the-compile-a86900de and habu-promote-the-tier-e94b65fc"
---

Problem: A64RA:ALLOCATE 39.2 s per load, vals^1.71 in the top decile; ANSWER-COUNT alone 7.1 s, ANSWER-RECORD 5.5 s, CHECK-GROUP 3.0 s; the 79 bodies over 256 values cost 25 s in allocation. The member-list change (cedar, parent 41df9051) removed global pairwise class scans for 7% on ANSWER-COUNT. Acceptance: allocation time fits a slope at most 1.1 against SSA values over the Tender per-definition set; every allocator negative kept; forced-AOT regalloc/loop/edge suites green; controlled pair. Files: src/compiler/native/regalloc.f. Verify: tender-perdef scaling fit; the allocator suites. Depends: none. Ownership: cedar. Claim: unassigned


Current ownership and handoff: Owner: cedar with /root/compiler_xhigh_review implementing the next regalloc.f-only optimization on 92ef13f0. Cross-reference habu-reduce-remaining-superlinear-9ece2395 for current measured history and diagnosis.
