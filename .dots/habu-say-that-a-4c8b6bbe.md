---
title: Say that a quotation may never return
status: open
priority: 2
issue-type: task
blocks:
  - habu-say-that-a-77c94eab
  - habu-keep-facts-through-beb96862
created-at: "2026-11-25T22:54:13.095744+01:00"
---

Problem: with ': GEN ( -- ) s" : MADE ( n -- n ) ;" evaluate-closed ;' GEN ': LATE ( -- ) 0 MADE drop s" x" 1 die ;', ': Q ( -- n ) [: LATE ;] execute 5 ;' is rejected by the load (E-DEAD-CODE at 5, 4:33: the quotation never returns) and accepted by tools/check.f --verify-only: the check cannot see MADE, so LATE may never return (CTL-MAYDEAD), but a quotation literal whose body ends there records no such fact, so execute returns normally. Acceptance: the check leaves the token after the execute to the load (W-CHECK-DEFERRED, DW-MAYDIE) and rejects nothing the load accepts; a diag-position case. Depends: habu-say-that-a-77c94eab, habu-keep-facts-through-beb96862. Claim: unassigned.
