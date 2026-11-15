---
title: Reject a type text past the cell cap, without garbage
status: open
priority: 2
issue-type: task
created-at: "2026-11-15T10:10:02.661038+01:00"
---

Problem: EFFECT:READ-TYPE of '[ ' + 100 x 'n' + ' -- ]' raises no error and the rejection's type text shows 64 cells; of '[ ' + 64 x (ptr x37 ' u8') + ' -- ]' it gives E-UNKNOWN-SIGNATURE-TYPE naming 'p\xF2&', garbage bytes. Cause not found. Acceptance: the cause is named; a type text past a cap is rejected naming the cap and the type's own text, never truncated silently or named by garbage. Depends: none. Claim: unassigned.
