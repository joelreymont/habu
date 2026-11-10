---
title: "Report the load's refusal for using-shadow in verify"
status: closed
priority: 2
issue-type: task
created-at: "2026-10-25T02:12:52.158927+01:00"
closed-at: "2026-11-10T21:50:20.947914+02:00"
close-reason: merged into habu-check-src-through-1198f4ed
---

Problem: 'using P' where P exports a global's name: the load refuses (bare word a global and a used package both export, rc 105); verify refuses with a different code (certified clean on 9f3fcb18). Acceptance: verify reports the load's refusal code for using-shadow; a check-verify-test row pins it. Verify: tools/check-verify-test.f.

Census 2026-11-04: a disagreement between two implementations of one thing; it is closed by its parent removing the second implementation, not by patching that copy.
