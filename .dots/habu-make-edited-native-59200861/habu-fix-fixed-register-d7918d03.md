---
title: Fix fixed-register loop refusal
status: active
priority: 2
issue-type: task
created-at: "2026-10-20T15:37:05.515619+03:00"
---

Public03d231f6 compiles the six-definition INSERT-EDIT reproducer, while public09fea843 and merged1228e908 refuse it at tier1 with E-A64RA-FIXED(-8460), exit67; tier0 succeeds. Exact body and wrappers are /Users/joel/.cache/tmp/kevin-insert-edit-repro. The pointer traversal and early-return branch obey checked stack contracts. Identify and repair the responsible native compiler/regalloc regression without caller rewrites or relaxed fixed-register constraints. Add coverage through the existing real native compile/run path before production changes, preserve actual sorted-list behavior, independently Astra-review and qualify the matching product before public promotion. Owner carl, isolated carl-native-fixed workspace.

Protected stores pin their copied addresses to x10. Coalescing compared member interference and merged disjoint pointer lifetimes into a hull spanning another x10 pin; placement correctly refused that hull. Keep the copy when the merged pinned hull overlaps another pin in the same register. The existing native-order-exit test now executes empty, prepend, equal-offset/rank middle, and append cases through that exact pointer traversal. A matching product built successfully and one process loaded the original native reproducer plus native-order-exit, native-regalloc and x64-regalloc: rc0, loaded INSERT-EDIT and three test:ok reports. Independent review and integrated release qualification remain lead-owned before closure.
