---
title: Compile native control flow
status: open
priority: 1
issue-type: task
created-at: "2026-07-26T22:56:29.042117+02:00"
---

Full context: design Wave 3 adds IF/ELSE, BEGIN loops, EXIT, and RECURSE through explicit block arguments and one exit block. Acceptance: zero-trip, back-edge, early-exit, nested, and loop-carried differential cases pass with no hidden virtual-stack snapshots.
