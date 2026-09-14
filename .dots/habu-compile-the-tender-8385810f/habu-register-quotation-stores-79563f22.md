---
title: Register quotation stores against shared image DATA
status: closed
priority: 1
issue-type: task
created-at: "\"\\\"2026-09-14T02:24:35.664285+03:00\\\"\""
closed-at: "2026-09-14T03:01:41.270479+03:00"
close-reason: Independent source review and all eight K2 storage/image suites pass, including exact worker ownership and unchanged concurrency stress. Registrar race 1420730a and the combined gate remain separate.
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

K caught a declaration error before emission: subtracting ptr n specialized the
store's quantified element type. The image is now viewed as bytes and only a
duplicate destination is converted with byte-view, preserving the value and
original pointer types. Exact helper source passes optimizing compilation in a
fresh package; independent review approved the correction. K2 is pending.

K2 source b8e069a5 built from J in 135.940 s, rc0, SHA256
6428d167d119e683322c7f4f17b28cb6f7d10e63ffe38087df49ed355228a68f.
Root's actual native-product checks all pass: image-lifecycle-tasks 0.614 s,
native-stored-quot JIT/AOT 0.514/0.965 s, address-cell-cap-grown 2.016 s,
address-cell-storage-oom 2.018 s, snapshot-xt-cell-decl 0.615 s,
snapshot-writer 18.949 s and app-image 59.716 s. The worker suite retains the
original four-worker growth/reuse stress alongside exact local/shared ownership
controls. Commands and logs: /tmp/cedar-K2-focused/results.json and adjacent
named logs. Independent source review and focused acceptance complete;
combined full-gate acceptance remains with the campaign. The public registrar
race is separately tracked in 1420730a and is not fixed by this owner change.
