---
title: Investigate MAIN+620 nil crash in habu0 CAR nil branch
status: closed
priority: 1
issue-type: task
assignee: ""
created-at: "2025-12-07T20:37:03.824487+02:00"
closed-at: "2025-12-08T14:07:51.14458+02:00"
close-reason: ""
---

Crash in habu0 native binary at MAIN+620 (EXC_BAD_ACCESS on CAR nil). Need to investigate null-check/codegen around tac-cmp/if and ARM64 neg encoding.
