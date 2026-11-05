---
title: Keep only the load-bearing tests
status: active
priority: 1
issue-type: task
created-at: "2026-11-05T17:06:50.749231+02:00"
---

Joel 2026-11-05: 'what does it do and why do we need it? does it carry its weight?' (prefix-provided-unfilled, deleted c32c4910), 'what else can and should be removed?', 'all tests slow down development so only the load-bearing tests and suites should be left'. Load-bearing: a test catches a concrete failure of code that stays, which no other test or the landing build catches. Measured (rowtimes of the 2026-11-05 L4b suite, 532 rows, 9739 s): 31 tests of 60 s or more are 76% of the time, 35 of 20-60 s 12%, 79 of 5-20 s 9%, 386 under 5 s 3%. Plan: wave 1 census of the 31 slowest (two read-only workers, briefs /tmp/jerry-prune/brief-w1a.md, brief-w1b.md), per case: what failure it catches, the code it guards and whether that code is being replaced, x86-64/Wasm-only or stays, its cost, its overlap, a verdict (keep, remove, cheapen); then removal lanes by verdict; waves 2-3 for the rest. Files: test/, tools/*test*.f, test/gate-*-cases.f. Verify: each lane's removals with the suite loading and the kept tests passing. Depends: none. Ownership: the test suite. Claim: agent=jerry workspace=.jj-ws.noindex/jerry-ppu (read-only census tree).
