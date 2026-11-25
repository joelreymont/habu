---
title: "Check a trusted definer's clause in the check"
status: active
priority: 2
issue-type: task
created-at: "2026-11-25T19:34:25.837270+01:00"
---

Problem: the load checks the does> clause of a TRUSTED: definer (a clause that ends with the wrong row is E-MISMATCH) while check.f --verify-only passes it: 'TRUSTED: D ( n -- ) create , does> ( ^n -- n ) dup ;'. Acceptance: load and check give one answer at one place; a test fails without it. Depends: none. Claim: agent=jerry workspace=.jj-ws.noindex/jerry-ck-def.
