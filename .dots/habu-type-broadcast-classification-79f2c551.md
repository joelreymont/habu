---
title: Type broadcast classification
status: open
priority: 2
issue-type: task
created-at: "2026-07-19T21:26:33.271052+02:00"
---

maki/bcast.f:23-38 returns five broadcast outcomes as raw n, with BC-ILLEGAL=-1. Every lowering caller can compare, store, or accidentally interchange this result with dimensions, indexes, op classes, and errors; the checker cannot force handling of every legal class or the illegal result. Declare a closed broadcast-classification ENUM with full, row, column, scalar, and illegal variants. Make BC-CLASS return it and require each lowering to MATCH exhaustively, mapping illegal to its own named error while selecting exact load-index logic for the four legal variants. Delete all numeric constants and default comparisons. Preserve degenerate-dimension precedence, shape legality, generated PTX, host/device parity, and caller-owned errors. Add checker negatives for raw n/rows/cols/foreign-enum use, exhaustive classification over normal and degenerate shapes, illegal dimensions, and byte-golden lowering for every class. Measure JIT/DATA/CODELEN and classification/lowering throughput before/after. Files: maki/bcast.f, lower/ew.f, lower/red.f and focused tests. Verify broadcast/elementwise/reduction/device suites, Maki, typed-local diff, type/package/host/dot lints, and full native gate. Ownership: broadcast result domain only.
