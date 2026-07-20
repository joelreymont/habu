---
title: Expose checked mmap release
status: active
priority: 1
issue-type: task
created-at: "\"2026-07-15T23:48:26.535083+02:00\""
---

Full context: lib/memory.f allocates OS mappings but exposes no checked release; lib/vector.f resize abandons the old mapping, so reusable compiler tools cannot own storage without leaks. Add native and Gforth-recovery munmap primitive parity, checker effect/model, packaged MEM:RELEASE-BYTES over typed pointer+allocation length, exact rc/error propagation, negative span/zero/size tests, bootstrap-codegen and fixpoint proof. Files: src/habu/habu2.f, bootstrap/cg/forth.fs, lib/memory.f, lib/memory-test.f, primitive/checker inventories and FILEMAP. Must land after habu-checker-reject-compile-c8805039 releases overlapping compiler/bootstrap files. Acceptance: allocate/write/release succeeds native+recovery; invalid/zero/forged sizes reject at the typed boundary; syscall failure propagates; no global MEM-* addition; focused memory/bootstrap/fixpoint plus typed/host/filemap gates green.

Claim: agent=mmap workspace=.jj-ws/fable-mmap machine=spark (owns MEM:RELEASE-BYTES munmap parity across src/habu/habu2.f + bootstrap/cg/forth.fs + lib/memory*.f + inventories; NOTE habu2.f/forth.fs also mid-edit by the direct-BL lane, orchestrator hand-merges. Stale-prose correction: the referenced prerequisite habu-checker-reject-compile-c8805039 no longer exists anywhere in .dots - constraint void)

macOS evidence for this lane (orchestrator, 2026-07-20): the region-move's EM-MMAP-CODE-REGION (src/habu/habu2.f ~3321) maps at hint __text+REGION-OFF WITHOUT MAP_FIXED and rejects any addr != hint as a collision. On macOS the kernel does not reliably honor a non-MAP_FIXED hint, so ANY engine boot can die "hb: cannot map fixed code region" (rc 78) nondeterministically - proven flaky (warm snapshot 4x: fail, fail, boot, fail), worse under pool concurrency; it flickers gate-stdlib phases (owner-wid-internal, build-fixpoint-fixtures, ptx-toolchain members) run to run. The addr==hint gate is too strict: the correct invariant is BL-range membership, which the code already asserts AFTER the strict gate. Fix shapes: accept any BL-range placement, or MAP_FIXED at a computed clear slot, or bounded retry - the baked-artifact side is already deterministic (the b0-relative AOT literal fix landed 2026-07-20).
