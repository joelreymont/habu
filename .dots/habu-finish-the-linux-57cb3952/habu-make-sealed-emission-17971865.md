---
title: Make publication and sites target-neutral
status: closed
priority: 2
issue-type: task
created-at: "2026-09-29T12:51:36.371760+03:00"
closed-at: "2026-09-30T17:46:00.000000+02:00"
close-reason: "complete: leaves 201c6fbf (P1), 729a7ac6 (P2) and d8c030e4 (P3) are closed and the lane holds no other work."
blocks:
  - habu-fill-nemit-from-d8c030e4
---

Lane P: publication and sites. Publication becomes byte-based and target-neutral (`src/compiler/native/publish.f:9,15,44-52,60-105`: `INSN-BYTES 4`, BL decoding in `RELOC-CALLS`, `size INSN-BYTES -` in `RECORDED-LEN`) through the `NEMIT` surface with an A64EMIT adapter (P1); x86 live-region sites become a row band in the bytes the ARM64 bitmaps occupy, read through one `SITES` reader (P2); X64EMIT fills `NEMIT` and publish accepts an x86 emission (P3). Spark. Serialise: `publish.f` and both `passes.f` files (P1, P3, X1); `layout.f` (P2, K2).
Leaves: habu-add-the-nemit-201c6fbf (P1), habu-represent-x86-live-729a7ac6 (P2), habu-fill-nemit-from-d8c030e4 (P3).
Campaign lane only; do not dispatch. It lists its leaves under blocks: so it stays off dot ready until they close.
Ownership: krait (Intel lane).
Claim: unassigned.
