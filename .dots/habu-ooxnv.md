---
title: Refactor habu0 to use arm64/asm.lisp instead of duplicate a64-* functions
status: closed
priority: 1
issue-type: task
assignee: ""
created-at: "2025-12-08T14:37:38.188297+02:00"
closed-at: "2025-12-09T09:28:47.631703+02:00"
close-reason: ""
---

habu0.lisp has duplicate ARM64 instruction encoders (a64-movz, a64-add-imm, etc.) that duplicate arm64/asm.lisp. This violates DRY and causes bugs when adding new instructions. Refactor so habu0 uses the canonical arm64/asm.lisp encoders.
