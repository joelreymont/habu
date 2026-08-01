---
title: Rebind a typed local in the native chain
status: open
priority: 3
issue-type: task
created-at: "2026-08-01T13:20:23.204488+02:00"
---

The native elaborator (src/compiler/native/elaborate.f) binds a {: ... :} local once, at the closer, and reads it by name; there is no rule for rebinding one. Habu spells a rebinding with a store-to-local word, which is not in the dialect's vocabulary (src/compiler/native/hir-word.f), so a body that rebinds is refused today as E-HIR-UNMODELED - correctly, but as 'this dialect cannot compile that word' rather than as the capability it is. Wanted: the word declared with a meaning of its own, and the elaborator overwriting the name's bound value with the value on top of the compile-time vector. A rebinding inside a control structure needs the name to travel as a block argument the way any other live value does, so do the top-level case first and refuse the nested one by name. No corpus word needs this: LERP, BYTE-SUM and BYTE-FIND all bind once and only read.
