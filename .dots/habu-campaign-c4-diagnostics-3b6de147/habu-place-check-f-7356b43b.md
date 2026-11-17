---
title: "Place check.f's record at its input line's own column"
status: open
priority: 2
issue-type: task
created-at: "2026-11-16T16:50:58.188685+01:00"
---

Problem: a record the run stage places at the '<input>' script-required line shows that line as the subject's 39-byte 's" F" script-required' but a column from check.f's own longer input: an FFI group held open across an include, before that was rejected, under tools/check.f printed <input>:1:61 with the caret past the line's end, where the load prints <input>:1:25. Acceptance: under tools/check.f such a record's line and column are the load's; a test pins it. Depends: none; that case is now rejected at the declaration's opener, so the fix starts from another record placed at the input line. Claim: unassigned.
