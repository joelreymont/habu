---
title: Initialize checker scratch after image restore
status: open
priority: 1
issue-type: task
created-at: "2026-09-24T17:18:43.268422+02:00"
---

TVT-BOOT, RVT-BOOT, EC-TV-BOOT and EC-RV-BOOT persist 5,120 UNBOUND (-1) cells. Their 40,960 raw bytes become 51,200 value bytes plus 640 bitmap bytes. They are per-definition scratch, but TV-SNAP-RESET deliberately leaves them UNBOUND while clearing high-water counters; later reset code clears only used prefixes. Simply capturing zeros is incorrect.

Restore the required initial state after DATA and pointer restoration and before any checker use, or replace the sentinel representation coherently. Omit reconstructible contents from the image while preserving fresh-process, first-definition, error recovery, growth and repeated snapshot/capture behavior. Respect both cold engine and warm snapshot startup paths.

Own checker.f TV-SNAP-RESET/reset consumers and the checker initialization/capture seam; update habu2.f startup only where necessary. Reuse existing checker/reset/rollback and image-lifecycle suites, exercising the actual first definition after restore. Measure image section reduction and startup cost separately. Native fixpoint, full test/run.f and Maki smoke are required before landing. No dependency on metadata pruning; coordinate the shared capture/restore seam. Baseline and RCA are recorded in the Optimization parent.
