---
title: "Hold an ELF32 segment's bytes as one str"
status: active
priority: 2
issue-type: task
created-at: "2026-11-22T16:39:22.198080+01:00"
---

Problem: ELF32:segment's data ^u8 / size NUM:byte-len fields (src/lib/elf32.f:37-38) spell a byte-string pair, so MAKE-SEGMENT converts with N>BLEN and elf32-test.f with BLEN>N; ELF32:FILE-BYTES (elf32.f:139) is called only by its own test (elf32-test.f:75). Acceptance: the segment holds its bytes as one str field (or the structure's str form), the conversions go, FILE-BYTES and its test case are deleted, the elf32 suite passes. Depends: none. Claim: agent=worker workspace=.jj-ws.noindex/jerry-p2elf
