---
title: Package lib/test/runner.f and retire its lint row
status: open
priority: 2
issue-type: task
created-at: "2026-08-21T10:43:30.460104+02:00"
---

The retirement owner for the exact-path GLOBAL-IMPLEMENTATION? row gtrc-1 is adding for lib/test/runner.f (2026-08-21, measured blanket freeze: editing one body line of an untouched word on pristine master reds E-PACKAGE-OWNERSHIP - the file opens no package and the gate refuses every possible edit, including the repair of the runner's evidence gap). The migration: package runner.f (86 global names) + runner-test.f, qualify/import the ~29 consumer files (test/run-lib.f, gate-pool.f, every gate-*-lib.f), per the seal campaign's using-import doctrine; then the row leaves and its positives become fixture negatives (asm.f precedent). Not small - own review.
