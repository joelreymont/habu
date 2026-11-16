---
title: "Place check.f's record at its input line's own column"
status: open
priority: 2
issue-type: task
created-at: "2026-11-16T16:50:58.188685+01:00"
---

Problem: a record the run stage places at the '<input>' script-required line shows that line as the subject's 39-byte 's" F" script-required' but a column from check.f's own longer input: /tmp/jerry-h8c/a2.f under 'hb --load tools/check.f -- a2.f' (installed hb) prints <input>:1:61 with the caret past the line's end, where the load prints <input>:1:25. Acceptance: under tools/check.f such a record's line and column are the load's; a test pins it. Depends: none; a2.f no longer reproduces it, since a load inside an open declaration is now rejected at the opener, so the fix starts from another record placed at the input line. Claim: unassigned.
