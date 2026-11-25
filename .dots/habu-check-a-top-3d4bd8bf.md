---
title: Check top-level code as the load does
status: active
priority: 2
issue-type: task
created-at: "2026-11-25T22:54:13.084751+01:00"
---

Problem: tools/check.f never judges the types of top-level code: SCAN-TOP (src/habu/verify-source.f 4517) sends each top-level token through TOP-OPERAND/TOP-TOKEN to TOP-RESOLVE and TOP-ANSWER (src/core/check-walk.f 2517), which only resolve the name and its require and visibility rules, and nothing records that types went unjudged, so --verify-only answers rc 0. The load judges each top-level step on a row of the live stack (src/core/top-row.f, check-walk.f 3379-3446). Each of these one-file programs is rejected by the load and passes the check silently: "' EVC U" with EVC rendering and U ( [ -- ] -- ) keeping its quotation (E-MISMATCH); "' N1 U2" with N1 ( -- n ) and U2 ( [ n -- ] -- ); "5 U3" with U3 ( bool -- ); "' EVC KV !" with KV a [ -- ] variable; 'variable V here V !' (E-RAW-CELL-PTR); '7 0 / drop' (E-DIV-ZERO). docs/forth-card.md says the check rejects these at top level (the section 3 table's source line, E-DIV-ZERO's row, section 5). Acceptance: the check judges top-level steps as the load does and leaves to the run, with W-CHECK-DEFERRED, only what it cannot model; each program above rejected by the check where the load rejects it, in a test that fails before. Depends: none. Claim: agent=jerry workspace=.jj-ws.noindex/jerry-tick.
