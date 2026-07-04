---
title: "TFAM 5: constant logical-shape parity in replay/public-sig"
status: open
priority: 2
issue-type: task
created-at: "2026-07-04T08:53:59.621169+02:00"
---

Stop hardcoding constant as -- a for layout values in verify-source.f (:381), tools/public-signatures-core.f (:552), and reconcile with all-errors which replays the literal 'N constant NAME' line (CA-ADD-SUPPORT-CONSTANT :357). Either reject layout constants consistently across all three replay paths or carry the full logical value shape. Add layout-producing constant fixtures proving none treat them as one-cell -- a. Part of item-5 replay parity; do via the shared event/replay framework.
