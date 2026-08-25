---
title: Add linear capture phases
status: open
priority: 1
issue-type: task
created-at: "2026-07-13T11:50:07.480526+02:00"
---

Full context: src/habu/aot-capture.f captures verbatim records, proves them, finalizes the boot manifest, emits, and seals through mutable buffers. Cause: the checker has no linear phase state, so buffer authority can be reused or advanced out of order. Fix: introduce linear package capabilities for records-proven, manifest-final, emitted, and sealed; bind each region borrow to its legal phase; reject reuse, out-of-order transitions, and escape across throw or rollback. Acceptance: the current AOT buffer-aliasing sequence rejects at CHECK!; the only legal phase chain is records-proven -> manifest-final -> emitted -> sealed, with each transition consuming exactly one prior capability; exception cleanup is proven; native/recovery snapshot and fixpoint gates pass.
