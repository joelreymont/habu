---
title: Compact the effect store to the kept symbols
status: open
priority: 1
issue-type: task
created-at: "2026-09-30T18:50:15.052768+02:00"
---

Problem: after 974304d0 retires the symbols, their 40-byte effect bindings and unshared contents and nodes are still in USIGS; headers alone are 165,219 of USIGS's 203,245 B. Zeroing in place breaks `UIX-REC-ADD` (`checker.f:6584`), which assumes a record's own bytes lie between it and the next record.
Acceptance:
- **Compaction:** a copying compaction marks from the kept bindings with the graph walk `tools/effect-store-census.f` WALK (line 170) already implements. It copies the kept records, and the contents and nodes they reach, into a fresh store in their original order, primitive rows first so `USIGS-USER-OFF` holds.
- **Remaps:** it remaps `ER.CONTENT`, `ER.SYMPREV`, `ER.NEXT`, the `EC.*` row offsets, the node link fields, `PE.EFF` and `NORET.CREATES`, then swaps `USIGS-P`/`UEND`.
- **Equivalence check:** in process, for every kept symbol, `EFFECT-QUERY`, the control flags, the defer flag, the creates effect and `SIG-MIN-IN` are equal before and after.
- **First, one hour:** evaluate whether the mode-1 payload export/import (`CHECKER-PAYLOAD-ARM`, `PAYLOAD-SPANS`) already carries control flags, defer flags, creates and width facts. If it does, reuse it instead of writing a compactor.
Files: `src/core/checker.f`, `src/core/checker-surface.f`, `tools/effect-store-census.f`.
Verify: the equivalence check; `test/run.f`; generations byte-identical; the effect-store census bindings equal the kept-symbol bindings.
Depends: habu-drop-private-signatures-974304d0.
Parent: habu-ship-only-the-d7d38629. Design: the Fable surface design of 2026-09-30 (~/.cache/tmp/heron-arm64/design-surface.md); census: ~/.cache/tmp/heron-arm64/size-census/.
