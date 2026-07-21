---
title: Add owned growable byte buffer
status: closed
priority: 1
issue-type: task
created-at: "\"2026-07-15T23:49:39.985507+02:00\""
closed-at: "2026-07-21T13:45:35.058560+02:00"
close-reason: "Landed 19aafde7: the owned growable byte buffer (package BUF, lib/byte-buffer.f) - the reusable storage piece the build-tooling chain was missing. Caller-embedded 3-cell header with the capacity cell as the ownership token (the landed vector discipline mirrored for bytes): copy-on-grow with release strictly after successful install, consume-before-release dispose making double-dispose a proved no-op, doubling growth with a fail-closed wrap guard - and a genuine correctness finding: the magnitude capacity ceiling vectors need is structurally dead for byte lengths, so the live guard is the arithmetic-wrap check, tested directly, dead arms removed. One audited trusted projection owned by the standing modular-build epic; all lints green; 2 MiB chunked-fill and random-append property proofs; red-first mutation proofs on the copy and the ownership-clear. Full tests green at the merged tip over the Mac's certify-cycle repair. Indexed byte access deferred until a real consumer"
blocks:
  - habu-expose-checked-mmap-06c1d522
---

Full context: source composition and diagnostic tools use fixed byte arenas or one mmap per fragment. Add one package-owned growable byte buffer with data/length/capacity state, checked doubling and cell overflow, append byte/span, reserve exact, clear-for-reuse, replace, and dispose through MEM:RELEASE-BYTES. Public API uses typed byte length/capacity roles; project words uppercase and no raw prefix globals. Acceptance: zero/overflow reject, growth preserves bytes, clear reuses allocation, growth releases the prior mapping, dispose is exact, multi-megabyte and property tests pass. Files: one new lib byte-buffer concern plus test, manifest and FILEMAP.

Claim: agent=growbuf workspace=.jj-ws/fable-growbuf machine=spark (owns the owned growable byte buffer lib module + tests)
