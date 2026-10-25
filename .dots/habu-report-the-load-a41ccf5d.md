---
title: "Report the load's refusal for using-shadow in verify"
status: open
priority: 2
issue-type: task
created-at: "2026-10-25T02:12:52.158927+01:00"
---

Problem: 'using P' where P exports a global's name: the load refuses (bare word a global and a used package both export, rc 105); verify refuses with a different code (certified clean on 9f3fcb18). Acceptance: verify reports the load's refusal code for using-shadow; a check-verify-test row pins it. Verify: tools/check-verify-test.f.
