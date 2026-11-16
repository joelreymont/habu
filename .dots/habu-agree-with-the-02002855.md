---
title: Agree with the load after an OPAQUE misspelling
status: open
priority: 2
issue-type: task
created-at: "2026-11-17T07:23:10.699082+01:00"
---

Problem: package QC / OPAQUE STRUCTURE box FIELD x n ;STRUCTURE / private / : BOX-MAKE ( -- n ) 1 ; / ;package: the load stops at E-UNDEFINED OPAQUE 2:1, but tools/check.f --verify-only adds E-DUPLICATE-DEFINITION at 4:3 (the d5135 re-review; master behaves the same). Acceptance: all three readers give the load's one record. Depends: none. Claim: unassigned.
