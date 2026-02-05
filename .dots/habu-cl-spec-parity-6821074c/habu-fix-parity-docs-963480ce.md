---
title: Fix parity docs
status: open
priority: 1
issue-type: task
created-at: "2026-02-05T12:11:01.492519+01:00"
---

docs/cl-spec-status.md: replace incorrect '100% complete' claims; docs/PROGRESS.md: reconcile counts vs docs/cl-symbols.md; docs/cl-symbols.md: fix header totals (implemented/partial/missing) + note source-of-truth. Root cause: docs drifted + multiple trackers. Fix: generate summary from docs/cl-symbols.md via tools/cl_audit.zig and reference it; update docs accordingly.
