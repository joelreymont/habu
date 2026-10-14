---
title: Reuse independent FILE units after edits
status: open
priority: 1
issue-type: task
created-at: "2026-10-14T16:55:46.372127+03:00"
---

Otto’s public436ff134 Dots reproducer (/tmp/otto-incremental.md and /tmp/otto-incremental-repro.sh) reports44.8–63.2s edits; an early main.f edit recompiles17 unchanged library units because FILE keys contain preceding build history. Fix at compiler/cache reference and dependency boundaries: relocate actual bound code/record/WID/DATA identities and record consumed contracts, values and executed producers. Preserve redefinition/private alias semantics and refusals. Acceptance: real early/middle/late app and engine edits reuse unchanged independent FILEs, execute edited behavior, preserve rejected programs, and measure complete invocation. Full native suite, uncached convergence and independent Astra review before landing.
