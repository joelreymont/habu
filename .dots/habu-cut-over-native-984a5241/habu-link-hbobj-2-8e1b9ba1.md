---
title: Link HBOBJ targets
status: open
priority: 1
issue-type: task
created-at: "2026-07-26T23:00:00.461901+02:00"
blocks:
  - habu-define-canonical-hbobj-c2cd2186
---

Full context: Wave 7 requires Mach-O and ELF consumers of validated HBOBJ with explicit code/data addresses and relocations. Acceptance: target legality, relocation ranges/kinds, symbol visibility, source maps, executable protection, and malformed-object negatives pass on macOS/Linux AArch64 fixtures; no instruction decoding.
