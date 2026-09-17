---
title: Ratchet the trivial-definition compile floor
status: closed
priority: 2
issue-type: task
created-at: "2026-09-11T16:38:06.286337+03:00"
closed-at: "2026-09-11T19:05:39.100007+03:00"
close-reason: "Landed on the root: tools/compile-floor.f and test/compiler/compile-floor.f (registered as compiler-compile-floor). Real-dispatch timing with an NCOMP count proof, ratchet on a ms argument, prior dispatch xt and tier restored with finally on both outcomes (review blocker fixed, controls committed). Root b4363e71 measures trivial-t1 4,111 us, three-op-t1 3,146 us, trivial-t0 41 us at load 5.8; the campaign base 41df9051 measured 9,095-9,123 us. Target 500 us."
---

Problem: 100 definitions of the form : Tn ( n -- n ) 1 + ; take 9.09 ms each through the optimizer (6.83 ms for a three-op body without a combine rewrite); the JIT takes 0.07 ms. Acceptance: a test tool that compiles the 100 trivial definitions through NCOMP:COMPILE and reports the mean, and a ratchet that fails when the mean exceeds the recorded floor; floor recorded at each landing of a sibling dot until it is under 0.5 ms. Files: tools/ or test/compiler/. Verify: run the tool on 41df9051 (about 9 ms) and after each landing. Depends: none. Ownership: rowan. Claim: unassigned


Independent Astra review of d62b1e79 is blocked: compile-floor.f installs COUNTING-COMPILE at file load and never saves/restores the old dispatch XT; MAIN also leaves tier0 selected, and a tier1 load can include later tool helpers in its accumulated count. Rowan owns the correction. Install/reset only after CONFIG inside MAIN, save dispatch and tier, restore both via finally on success and failure; add in-process restoration controls. The measured sets otherwise enforce100/100 optimizing dispatches and0 JIT dispatches. Default mode is report-only; optional threshold is positional milliseconds. Neither test/compiler/compile-floor.f nor test/tier.f is registered in test/gate-stdlib-cases.f yet. Do not integrate the current wrapper leak.
