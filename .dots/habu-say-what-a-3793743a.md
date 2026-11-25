---
title: Say what a breakpoint arm rejects in gate-debug-lib
status: open
priority: 3
issue-type: task
created-at: "2026-11-25T17:15:11.058309+01:00"
---

Problem: test/gate-debug-lib.f's comment above GDB-BREAKPOINT-REJECTION says an engine-text target can share a page with patch32 and that opening it removes execute permission from the patcher; breakpoints are trampolines out of line now and the claim is false. The comment and the word say rejection. Acceptance: the comment states what the test proves today, and the word and its labels say rejection; gate-debug-lib's suites pass. Depends: the x86 stack's landing. Claim: unassigned.
