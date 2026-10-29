---
title: Split DICT out of XREF
status: open
priority: 2
issue-type: task
created-at: "2026-10-29T13:08:16.326466+02:00"
blocks:
  - habu-build-a-tree-d00185b1
---

Joel 2026-10-29: package DICT (src/habu/dict.f) is the base layer that reads and finds dictionary entries: today's ~60 global XREF-* words (1590 uses, 128 files) become DICT:*, primitives xref-* become dict-*, and the compiler's entry readers move in from NDICT. XREF keeps only public REPL words (undefine), built on DICT. NDICT is deleted with tier 1.
