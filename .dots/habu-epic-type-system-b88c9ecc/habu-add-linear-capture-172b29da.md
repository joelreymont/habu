---
title: Add linear capture phases and one-shot hooks
status: open
priority: 1
issue-type: task
created-at: "2026-07-13T11:50:07.480526+02:00"
blocks:
  - habu-add-bounded-host-b40b048f
---

Full context: src/habu/aot-capture.f captures verbatim records, proves them, freezes owner rows, builds a boot manifest, emits, and seals through mutable buffers and quotation hooks whose stack type is only ( -- ); today OWNER-WID-CAPTURE:FREEZE can overwrite AOT-REC-BUF before later ACAP-BOOTRUN-NAME+ scanning. Cause: the checker has no linear phase state or one-shot hook lifecycle, so buffer authority can be reused and hooks can run, reset, or escape in the wrong order. Fix: introduce linear package capabilities for capture-open, records-proven, owner-frozen, manifest-final, emitted, and sealed; type hook install/run/reset as single-consume transitions; bind each region borrow to the legal phase; reject reuse, out-of-order transition, duplicate hook execution, and escape across throw/rollback. Acceptance: the current owner/AOT aliasing sequence and hook reorder mutations reject at CHECK!; each legal pipeline transition consumes exactly one prior capability; exception cleanup is proven; native/recovery snapshot and fixpoint gates pass.
