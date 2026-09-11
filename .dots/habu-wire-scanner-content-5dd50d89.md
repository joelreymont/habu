---
title: Wire scanner content provider
status: open
priority: 1
issue-type: task
created-at: "2026-07-17T13:09:24.741311+02:00"
blocks:
  - habu-tools-frame-diff-e98f8a6a
---

Review blocker: diff-capture production leaves the default content provider installed, so every CLI capture fails; tests use a per-row two-process provider and do not prove the bulk wire. Implement one checked adapter that invokes the M4 scanner once, validates HABUSIDE metadata/path/kind/order/duplicate binding, populates every content row, propagates scanner and FS outcomes, and rejects missing/extra/reordered/corrupt rows. No compatibility or per-row fallback. Acceptance: production CLI captures real jj changes with constant process count and all hostile binding fixtures.
