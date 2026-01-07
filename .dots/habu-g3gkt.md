---
title: Fix MCP disassembler to show XZR not SP for SUB shifted register
status: closed
priority: 2
issue-type: bug
assignee: ""
created-at: "2025-12-08T17:53:41.264736+02:00"
closed-at: "2025-12-08T18:10:04.890554+02:00"
close-reason: ""
---

The disassembler incorrectly shows SP instead of XZR for SUB (shifted register) when Rn=31. In ARM64, register 31 interpretation depends on context: for SUB shifted register, Rn=31 means XZR (zero register), not SP. This causes confusion when debugging generated code.
