---
title: Six arena families still persist host pointers
status: open
priority: 2
issue-type: task
created-at: "2026-08-18T19:43:25.676410+02:00"
---

P1, destruction review 2026-08-18 (corrects my overstated close on 17a8c792 - one INSTANCE eliminated, not the class): six src/core arena families carry the exact DEV-A-P shape (mmap-grown X-P cells, no SNAPSHOT-RESET, not persisted by CHECKER-SNAPSHOT-PREPARE): WFS-P (checker.f:8675 wide-field scratch), LOC-HW-P (:9183 locals high-water), TDPV-*-P (sumtype.f:1305 - the reset in the SAME FILE covers CTOR-PEND/TDPLAN but skips TDPV), BUF-P (lower-cert-base.f:40), twelve layout-valid cells (layout-valid.f:14-47), ARM-P (generated-declaration.f:701). If any grows during a snapshot build, the image persists a build-process mmap address = the DEV crash class, nondeterministic under ASLR - green today only because builds never outgrow boot caps, the exact green-by-accident docs/debugging.md:191 refuses. Fix per the DEV precedent: each family gets its SNAPSHOT-RESET in the existing chain (participant order), OR prove structurally it cannot grow pre-snapshot (a cap assert at snapshot beats a reset for a family that must be empty). The ASLR-intersect is the acceptance: the persisted set must contain ZERO mmap-band addresses, asserted as a snapshot-writer case so the class stays closed. Verified reset/persist inventory is in the review (task output); do not re-derive.
