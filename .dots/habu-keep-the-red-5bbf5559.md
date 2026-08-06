---
title: Keep the red-phase list out of the shared cache
status: open
priority: 3
issue-type: task
created-at: "2026-08-06T17:55:27.279486+02:00"
---

test/run-lib.f:1383 RED-LIST$ puts gate-red-phases.txt under PERSIST$, which TR-PERSIST-DEFAULT (line 415) resolves to XDG_CACHE_HOME/habu-gate or HOME/.cache/habu-gate -- a machine-global path, NOT under HB_TMP. Every workspace on the host writes the same file, so the last red run anywhere clobbers every other lane's --rerun-failed list and a lane can rerun phases that were never red on its own tree. Observed 2026-08-06: the file held a phase-9 line at 17:35 while the reported symptom was a phase-20 label, and a later two-run experiment overwrote it again with phases 35 and 9. TR-RED-PERSIST only writes on red and never clears on green, so a stale list also outlives the failure it describes. The list describes one run of one tree and belongs under that run's GT-ROOT; if a cross-run list is wanted it needs a per-workspace key. Found while attributing habu-attr-the-candidate-4a2356c5.
