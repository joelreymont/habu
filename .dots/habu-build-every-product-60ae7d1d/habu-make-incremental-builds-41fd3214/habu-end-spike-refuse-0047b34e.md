---
title: End spike-refuse diagnostic lines with a newline
status: closed
priority: 2
issue-type: task
created-at: "2026-10-30T21:55:04.662733+02:00"
closed-at: "2026-10-31T20:52:55.516023+02:00"
close-reason: "Obsolete: spike-refuse 0 hits; REFUSE-LINE and FILE-LINE end with a newline."
---

Problem: under HABU_NATIVE_CACHE_DIAG the spike-refuse line (FRAME-WHY? / FRAME-WHY+, tools/native-unit-object.f ~286-391 on zxztprpo) ends without a newline, so the next native-file line runs onto it (seen as 'spike-refuse run-reaches-kernel-word word=USE-INTERPRETER' joined to a frame line). Acceptance: every diagnostic line ends with one newline; the frame/auto e2e line readers see separate lines. Files: tools/native-unit-object.f. Verify: a warm diag build, grep shows no joined lines. Depends: lane 11. Ownership: frame test-edit lane. Claim: unassigned.
