---
title: Expose checked mmap release
status: open
priority: 1
issue-type: task
created-at: "2026-07-15T23:48:26.535083+02:00"
---

Full context: lib/memory.f allocates OS mappings but exposes no checked release; lib/vector.f resize abandons the old mapping, so reusable compiler tools cannot own storage without leaks. Add native and Gforth-recovery munmap primitive parity, checker effect/model, packaged MEM:RELEASE-BYTES over typed pointer+allocation length, exact rc/error propagation, negative span/zero/size tests, bootstrap-codegen and fixpoint proof. Files: src/habu/habu2.f, bootstrap/cg/forth.fs, lib/memory.f, lib/memory-test.f, primitive/checker inventories and FILEMAP. Must land after habu-checker-reject-compile-c8805039 releases overlapping compiler/bootstrap files. Acceptance: allocate/write/release succeeds native+recovery; invalid/zero/forged sizes reject at the typed boundary; syscall failure propagates; no global MEM-* addition; focused memory/bootstrap/fixpoint plus typed/host/filemap gates green.
