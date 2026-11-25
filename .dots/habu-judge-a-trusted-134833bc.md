---
title: "Judge a trusted definer's head in the check"
status: open
priority: 2
issue-type: task
created-at: "2026-11-25T20:29:02.248220+01:00"
---

Problem: 'TRUSTED: D ( n -- ) 0 if create , does> ( -- n ) @ ;' is E-T0-CONTROL at its does> in the load and rc 0 in tools/check.f --verify-only, since src/habu/verify-source.f SCAN-TRUSTED-BODY judges no token of the head. Acceptance: load and check give one answer at one place for a does> inside a control structure of a TRUSTED: head; a diag-position case fails without it. Depends: none. Claim: unassigned.
