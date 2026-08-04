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

Current `PROT-GUARD` protects fixed DATA/dictionary bands, but a growable
immutable arena is not authoritative while raw stores, copies, atomics,
syscalls, FFI out-pointers, `MAP_FIXED`, `mprotect`, or snapshot rebasing can
overlap, remap, or make its pages writable. OS faults alone do not provide
checked ownership, replay, or diagnostics.

Add a sealed `PROTECTED-SPAN` owner with page-aligned index-based records,
transactional non-overlapping registration, a linear writable-build lease,
atomic freeze to read-only plus permanent registration, and range-intersection
guards at every native and recovery mutation/remap sink. Records and guards
survive rollback, snapshot, AOT, fixpoint, and bootstrap; spans never expose
pointers as authority and cannot be unregistered or reuse IDs.

Acceptance: exact and adjacent spans succeed; duplicate, overlap, wraparound,
unaligned, stale lease, double freeze, capacity, and allocator failures reject
before publication. Start-inside, start-below-straddling, FFI high/sret,
syscall output, atomic/copy, `MAP_FIXED`, writable `mprotect`, snapshot rebase,
and recovery mutations reject with named diagnostics before bytes change.
Mutable build pages work until freeze; frozen reads and canonical restore work;
hostile raw writes cannot corrupt or revoke protection.

Files: the dedicated protected-span owner and focused tests plus only the
native/recovery store, atomic, syscall, FFI, mmap/mprotect, and snapshot guard
seams. Any surviving source `TRUST` keeps only its source-local rationale,
retirement owner, and focused production test. Run the red-first sink matrix,
native/bootstrap/AOT/snapshot/fixpoint, typed-local and package gates, then the
full native gate. Coordinate with, rather than duplicate, the fixed-range
hardening dependencies.
