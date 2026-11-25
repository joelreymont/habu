---
title: Check sees an unset defer in an immediate
status: open
priority: 2
issue-type: task
created-at: "2026-11-25T15:59:32.542751+01:00"
---

Problem: defer D ( -- ) : I ( -- ) D ; immediate : U ( -- ) I ; is E-DEFER-UNSET at the load and accepted without a warning by the check. Acceptance: the check rejects or warns at the immediate's use. Depends: none. Claim: unassigned.
