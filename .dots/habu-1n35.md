---
title: Fix wrapper BL offset jumps to NOP instead of MAIN
status: closed
priority: 1
issue-type: bug
assignee: ""
created-at: "2025-12-08T10:21:43.685922+02:00"
closed-at: "2025-12-09T09:29:52.85646+02:00"
close-reason: ""
---

Wrapper BL instruction has offset 4 but needs 5 after symtab storage was added. BL jumps to NOP at 0x7C instead of MAIN at 0x80.
