---
title: Fix mformat ~M directive under canonical Maxima path
status: open
priority: 3
issue-type: task
created-at: "2026-03-07T19:34:58.966858+01:00"
blocks:
  - habu-decompose-remaining-per-0c9e465d
---

../maxima/src/mformt.lisp, ../maxima/src/mforma.lisp, and Habu display/output path. Root cause: ~M formatting still crashes in Maxima output paths even after displa improvements. Fix: close the directive-specific bug under the canonical runner/image, not as a standalone legacy track. Why: output correctness belongs in the unified Maxima tree.
