---
title: Take the address of a typed local in the native chain
status: open
priority: 3
issue-type: task
created-at: "2026-08-01T13:20:42.083373+02:00"
---

A {: ... :} local in the native elaborator is a named SSA VALUE and lives in no memory at all, which is what makes reading one free. Taking its address would force it into a frame slot, so the elaborator would have to decide which locals are address-taken, reserve slots for them, and lower every read and write of those to a64.str/a64.ldr - the frame machinery the register allocator already uses for spills. Refused today as E-HIR-UNMODELED, because no address-of word is in the dialect's vocabulary (src/compiler/native/hir-word.f). No corpus word needs it.
