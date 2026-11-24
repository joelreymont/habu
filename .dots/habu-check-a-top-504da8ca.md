---
title: Check a top-level store into a typed variable
status: open
priority: 2
issue-type: task
created-at: "2026-11-24T20:21:23.647202+02:00"
---

Problem: at top level, `' X V !` stores an execution token whose row differs from the typed variable V's and the check passes it; the same store inside a colon definition is refused with E-MISMATCH. Reproducer: require src/lib/ffi-callback.f, then `: CBV-STEP ( n -- n ) 1 + ;`, `CALLBACK: CBV ( n n -- n ) 0 FALLBACK ;CALLBACK`, `' CBV-STEP CBV-BODY !`: no error. Acceptance: the top-level store is refused as the colon-definition store is. Depends: none. Claim: unassigned.
