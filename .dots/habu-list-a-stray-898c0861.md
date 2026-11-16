---
title: "Reject a stray top-level ; in the check"
status: open
priority: 2
issue-type: task
created-at: "2026-11-16T08:57:47.626529+01:00"
---

Problem: the load rejects a top-level `;` with no definition open (E-NO-DEFINITION, "`;` ran with no definition open"), but the composition walk passes it, so tools/check.f judges past it: `;` then `qqq` on the next line loads with E-NO-DEFINITION at 1:1, while check.f gives E-UNDEFINED `qqq` at 2:1, and --all-errors lists no `;` record (installed hb). Acceptance: check.f gives the load's record for a stray top-level `;`, and --all-errors lists it in source order; a test pins it. Depends: none. Claim: unassigned.
