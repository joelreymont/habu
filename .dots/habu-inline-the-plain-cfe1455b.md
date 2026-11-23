---
title: Inline the plain store in tier 0
status: open
priority: 2
issue-type: task
created-at: "2026-11-23T19:36:20.276747+02:00"
---

Problem: tier 0 compiles `!` as a call (DO-STORE src/habu/t0.f:1306; SET-CELLS :2427 says T0 has no inline store) while `@` is inlined (FETCH :575 through EMIT-LOAD, src/habu/t0-emit.f:360); the emit tables have no store. Profiling Omabox's journal-record decoding on Habu 6c0d2f62 put 10.3% of samples in `!` itself and 1.9% in `c!`, called from the JSON reader's state words, where a token costs about 62 ns. Acceptance: tier 0 compiles a one-cell `!` and `c!` as a store instruction on each target, with no call; a profile of the same decode shows no samples in `!` and the time per token down by its share. Depends: none. Claim: unassigned.
