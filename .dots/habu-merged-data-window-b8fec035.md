---
title: Merged DATA window is appended unaligned
status: open
priority: 2
issue-type: task
created-at: "2026-08-16T02:00:18.215234+02:00"
---

Proven by bake-chain-10 (2026-08-16): aot-file.f MERGE-DSITES appends the artifact's DATA window at H-DATA with no alignment, shifting every merged chain DATA address by a non-multiple of 8. Measured: A64RAV:DKEEP-HOOK's cell at 0x4400061d60e (aligned=NO) in the merged engine vs 0x...628 aligned in the source-loaded chain. Self-consistent today (written and read at the same skewed address) so not the current crash - but LDAR/STLR and atomics FAULT on misaligned addresses (SEAL-WIDS, already uses LDAR/STLR on the protected band; any chain word using atomics on its own DATA will die), and post-seed DP inherits the skew for every later allocation. Fix: align the merged window base up to 8, pad the content, and extend the merge suite's sum checks to prove the pad (merged DATA size = artifact size + pad, all rebased addresses 8-aligned when their source was). Regression: an alignment assert over the merged engine's declared cells.
