---
title: Compile native control flow
status: closed
priority: 1
issue-type: task
created-at: "\"2026-07-26T22:56:29.042117+02:00\""
closed-at: "2026-08-14T11:51:32.313882+02:00"
close-reason: "Epic closed children-first (Wave-3 audit 2026-08-14): all five children closed (compare f8b5d801 superseded; branch 1a7087bd, loop 71d4a638, lower 30d1a985, metrics a01a8ad7 satisfied). The epic acceptance - zero-trip, back-edge, early-exit, nested, loop-carried differentials with no hidden virtual-stack snapshots - is the block-argument construction itself plus the registered differential families in both gate surfaces and the 46-row board; re-proved live this audit (10/10 pairs agree through the production entry). Closed on the children, not on the exhausted edge set."
---

Full context: design Wave 3 adds IF/ELSE, BEGIN loops, EXIT, and RECURSE through explicit block arguments and one exit block. Acceptance: zero-trip, back-edge, early-exit, nested, and loop-carried differential cases pass with no hidden virtual-stack snapshots.
