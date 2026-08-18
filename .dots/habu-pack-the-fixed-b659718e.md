---
title: "Pack the fixed-width rows: 603KB of zero outside the window"
status: open
priority: 2
issue-type: task
created-at: "2026-08-18T20:11:07.188499+02:00"
---

The census's second target (2026-08-18): 603,361 zero bytes OUTSIDE the DATA window, inside fixed-width rows - reg 81% zero, records 57%, sigs 50%, call sites 48%, pwid 98.6%. This is PACKING, not boot-allot: variable-width or bitmap-present encodings per row family, each a format change (rides a VERSION bump - batch with the DATA-collapse migration or the binary-type-info migration, never alone). Measure per family first: bytes saved vs decoder complexity; the pwid bitmap (98.6% zero) may become a sparse list for near-free. Post-collapse work; the DATA window comes first.
