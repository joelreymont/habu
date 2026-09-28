---
title: Attribute the captured heap and drop sealed-dead records
status: open
priority: 2
issue-type: task
created-at: "2026-09-17T12:39:26.146860+03:00"
---

## Current attribution and task ownership

The qualified macOS engine at `e337d11fc098`, SHA-256
`24003f017a601713c84185a7be5b1e0664d9951c942847372d2a0ddb9b0a654b`,
contains 688,080 encoded DATA value bytes and 47,928 bitmap bytes. The decoded
span is 5,945,872 bytes. The DONE label is a nearest-owner interval, not evidence
that the REPL owns the anonymous checker stores. Earlier effect/control counts
in the RCA index are historical and must be remeasured after history compaction.

The current source audit confirms whole-interval capture after owner cleanup,
inactive rollback/MATCH buffers that are repointed without clearing, and growth
or truncation paths that can leave abandoned DATA bytes. Their contribution to
this exact product is unmeasured. EI-AK's 64 negative sentinel cells cost 640
value bytes; symbol strings have 21,097 bytes of cell-varint overhead over their
168,771-byte used extent. Neither figure proves private symbols removable.
Package spellings are already interned. A read-only census is measuring exact
live prefixes, tails, frame buffers, duplicate spellings and graph allocations
against the physical image. Design and current source evidence:
`~/.cache/tmp/habu-captured-data-audit-20260928-01.md`.

This task owns durable attribution and measurement. Implementation owners
are habu-drop-private-signatures-974304d0 for interface retention,
habu-compact-checker-histories-3a1ce692 for histories,
habu-share-effect-headers-d26ffc89 for repeated semantic headers,
habu-store-checker-names-70a89ffb for strings/pointers and
habu-init-checker-scratch-4c2afab4 for reconstructible scratch.
Do not create competing implementations from this umbrella task.

## Earlier task context

Problem: the captured heap, everything above the last declared cell, is 1241138 bytes in 58035 runs with an image cost of 1705418 bytes, a third of the 5112000-byte line-head engine (tools/engine-size.f 2026-09-17, shown as owner DONE because the heap has no owner). Nothing says which allocator owns it: checker declaration records, symbol records (SYMS-BOOT 113678 bytes plus SYM-STR-BOOT 237517), dictionary bodies, IR tables, hash tables. And after the internal-mark seal no source can name a sealed-internal word, yet the product image still carries its checker declaration and symbols; 4016 records (195772 code bytes) are unreachable from the engine-entry roots and only the public dictionary surface reaches them. Acceptance: (1) engine-size attributes heap bytes by allocating structure (each long-lived allocator marks its region or the capture records HERE marks per subsystem) and lists the top owners; (2) a measurement states how many bytes are declaration and symbol records of sealed-internal words; (3) the product image drops those records the way it drops their names, the whitebox image keeps them, with the measured size change stated, or the dot records the reason it cannot; byte fixpoint; test/run.f; whitebox suites through WHITEBOX-SUITE. Files: tools/engine-size.f, src/core/checker.f (SYMS-BOOT, declaration records), src/core/internal-mark.f, src/habu/aot-capture.f. Verify: engine-size; fixpoint; test/run.f. Depends: habu-strip-the-names-89d6524a for the sealed-internal set. Ownership: engine size. Claim: unassigned.
