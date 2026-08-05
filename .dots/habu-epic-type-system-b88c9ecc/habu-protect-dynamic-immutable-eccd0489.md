---
title: Protect dynamic immutable spans
status: open
priority: 1
issue-type: task
created-at: "2026-07-13T15:25:24.043839+02:00"
blocks:
  - habu-seal-syscall-full-f388a698
  - habu-snap-rebase-straddling-f0e97b92
---

Full context: current PROT-GUARD protects a few fixed DATA/dictionary bands, but a growable immutable arena cannot be an authority while raw stores, copies, atomics, syscalls, FFI out-pointers, MAP_FIXED, mprotect, or snapshot rebasing can overlap, remap, or make its pages writable. OS read-only faults alone are nondeterministic process symptoms and do not provide checked ownership, replay, or diagnostics. Fix: add a sealed PROTECTED-SPAN owner with page-aligned index-based span records, transactional non-overlapping registration, a linear writable-build lease, atomic freeze to read-only plus permanent registration, and range-intersection guards at every raw mutation/remap sink in native and recovery paths. Records and guards persist through rollback, snapshot, AOT, fixpoint, and bootstrap; spans never expose pointers as authority and cannot be unregistered or have IDs reused. Acceptance: exact/adjacent spans succeed; duplicate, overlap, wraparound, unaligned, stale lease, double freeze, capacity, and allocator failure reject before publication; start-inside, start-below-straddling, FFI high/sret arg, syscall output, atomic/copy, MAP_FIXED, writable mprotect, snapshot rebase, and recovery mutations reject with named diagnostics before bytes change; legal mutable build pages work until freeze; frozen reads and canonical restore work; hostile raw writes cannot corrupt or revoke protection. Files: dedicated protected-span owner and tests; native/recovery store, atomic, syscall, FFI, mmap/mprotect, snapshot guard seams only; docs/debugging/effects and TRUSTED as required. Verify: red-first sink matrix, native/bootstrap/AOT/snapshot/fixpoint, trust/refine/typed-local lints, full gate. Coordinate rather than duplicate fixed-range hardening; depends on their complete range/FFI/snapshot guards.
