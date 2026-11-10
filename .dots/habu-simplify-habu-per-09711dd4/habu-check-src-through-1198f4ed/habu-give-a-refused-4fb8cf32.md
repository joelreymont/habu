---
title: "Give a refused does> clause's JSON record its declared effect"
status: open
priority: 3
issue-type: task
created-at: "2026-11-10T22:40:11.384810+02:00"
---

Problem: lane 308 (parked commit 8d6da143) found that the JSON record tools/check.f writes for a refused does> clause has no declared_effect, while a refused colon body's record has one. The renderer is now src/core/render.f:1846-1851, which prints declared_effect and declared_effect_source only when RECEFF-ON? is set and TOP-STEP is not; whether a refused clause's record still lacks them on master has not been run. Acceptance: the record for a refused does> clause carries the clause's declared effect and its source the way a refused colon body's record does, seen failing first; or the dot closes with the run below as evidence that it already does. Files: src/core/render.f, src/core/checker.f, tools/check-test-lib.f. Verify: first, 'bin/hb --load tools/check.f -- --json-errors mk.f' where mk.f holds ': MK ( n -- ) create , does> ( -- ) ;' and the line 'parses: MK 1'; read the clause's record. Then a case in tools/check-test-lib.f's suite. Depends: none. Ownership: unassigned. Claim: unassigned.
