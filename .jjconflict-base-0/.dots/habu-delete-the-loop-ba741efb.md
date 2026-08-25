---
title: "Delete the loop inventory's duplicate field constant"
status: open
priority: 2
issue-type: task
created-at: "2026-08-10T22:06:16.604910+02:00"
---

tools/codegen-loop-inventory.f:84 defines its own  IMM12-LIM duplicating A64ASM:IMM12-LIM; the pkgasm lane qualified that file's two encoder calls instead of importing precisely so the import would not mint an inner-scope shadow (reason in the source). Delete the duplicate, import or qualify consistently. Files: tools/codegen-loop-inventory.f. Depends: none.
