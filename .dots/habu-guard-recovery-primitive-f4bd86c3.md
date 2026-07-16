---
title: Guard recovery primitive name width
status: open
priority: 2
issue-type: task
created-at: "2026-07-17T03:25:48.199331+02:00"
---

Destruction review of checker preflight revision 71f0c56e found bootstrap/cg/forth.fs REG-ROOM? can accept primitive names longer than its 16-byte inline field, derive negative padding, and loop/fault in Gforth move. This is independent of compile-immediate preflight. Implement as a separate recovery-codegen change with exact boundary tests for 16 and 17 bytes, named fail-closed diagnostic, native/recovery parity, bootstrap-codegen and no-binary recovery gates. Do not bundle into checker-hook work.
