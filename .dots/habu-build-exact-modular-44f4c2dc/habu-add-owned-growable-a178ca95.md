---
title: Add owned growable byte buffer
status: open
priority: 1
issue-type: task
created-at: "2026-07-15T23:49:39.985507+02:00"
blocks:
  - habu-expose-checked-mmap-06c1d522
---

Full context: source composition and diagnostic tools use fixed byte arenas or one mmap per fragment. Add one package-owned growable byte buffer with data/length/capacity state, checked doubling and cell overflow, append byte/span, reserve exact, clear-for-reuse, replace, and dispose through MEM:RELEASE-BYTES. Public API uses typed byte length/capacity roles; project words uppercase and no raw prefix globals. Acceptance: zero/overflow reject, growth preserves bytes, clear reuses allocation, growth releases the prior mapping, dispose is exact, multi-megabyte and property tests pass. Files: one new lib byte-buffer concern plus test, manifest and FILEMAP.

Claim: agent=growbuf workspace=.jj-ws/fable-growbuf machine=spark (owns the owned growable byte buffer lib module + tests)
