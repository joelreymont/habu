---
title: "Stop a colon clause's check as its definer does"
status: open
priority: 2
issue-type: task
created-at: "2026-11-25T20:52:22.020765+01:00"
---

Problem: after a rejected does> clause of a colon definer, check.f --verify-only stops at the definer's first top-level call with W-CHECK-STOPPED 'no parses: row says what it reads', which is false, and never reports a later rejection: ': D ( n -- ) create , does> ( -- n ) @ 0= ; : Z ( -- n ) 1 ; 5 D W1 : BAD ( -- n ) 1 2 ;', one word a line. --all-errors reports E-UNDEFINED at D and W1 and BAD's E-MISMATCH. Acceptance: both modes report those four. Depends: none. Claim: unassigned.
