---
title: Keep checker packages out of E-UNDEFINED help
status: open
priority: 2
issue-type: task
created-at: "2026-11-18T07:30:13.568495+01:00"
---

Problem: E-UNDEFINED for a user's undefined word `a` helps with "`a` is public in `BODY-ITEMS`", pointing user code at a checker-internal package (seen on the fixture imm-dc-kept.f in workspace jerry-c9b under the c9 stack's engine). Acceptance: the help names only packages a user program may require; a test fails first. Depends: none. Claim: unassigned.
