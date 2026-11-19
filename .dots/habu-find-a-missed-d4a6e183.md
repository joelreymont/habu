---
title: Find a missed wordlist lookup without a re-index
status: open
priority: 2
issue-type: task
created-at: "2026-11-19T14:09:05.475414+01:00"
---

Problem: src/core/check-walk.f NS-FIND (:3688-3696) clears and rebuilds its index on a miss of a positive non-global wordlist, and the name-recording stack's src/habu/linker.f SPELL-LOOKUP runs it (WORDLIST-PACKAGE) for each looked-up name: 3000 search-wl lookups in a wordlist no package holds build in 3.77 s against 1.44 s with wid 0, about 0.8 ms each. Acceptance: a miss costs a probe, not a re-index; that case builds within 10% of the wid-0 one. Depends: habu-make-a-build-6931d6ea. Claim: unassigned.
