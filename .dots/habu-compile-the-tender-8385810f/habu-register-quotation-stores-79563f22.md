---
title: Register quotation stores against shared image DATA
status: active
priority: 1
issue-type: task
created-at: "\"2026-09-14T02:24:35.664285+03:00\""
---

Cedar owns QUOTATION-STORAGE persistence classification and native SNAP-RELOC:EMIT-MARK owner selection. Product J image-lifecycle-tasks deterministically crashes under normal ASLR; GDB stops at 0x418708 loading the address row count at task-local x20 + 0xa9ca0. Task DATA is 64 KiB, but STORE classifies its next 32 MiB as image memory. The transient hook buffer falls in that false extent. Use the existing fixed shared DATA mapping for image classification and row registration, retaining thread-local data-base for task state. Preserve persistent shared quotation stores made by workers. Acceptance: deterministic task-local and shared-image quotation stores, exact shared row ownership/deduplication, unchanged four-worker growth/reuse stress and existing capture/restore quotation tests. Evidence: /tmp/cedar-J-task-crash/gdb-aslr-on.log. Review existing registrar synchronization and record a separate issue only if a race is reproduced.


The focused patch uses the existing DATA-VA mapping for both classification and
MARK. MARK-HEADER reacquires its base after successful mmap/munmap because SYS,
clobbers x16 on both native targets. Independent Astra review approved the owner
selection and register lifetime; combined binary acceptance remains pending.
The new registered regression checks task-local storage and shared registration
without reducing the existing four-worker growth/reuse stress.

A separate registrar race is now reproduced by the registry lane: simultaneous
first declarations lose rows even below inline capacity. Its fix belongs to the
shared registrar, alongside explicit row ownership, and is tracked separately.
