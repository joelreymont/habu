---
title: Cosine-LR + grad-clip for SGD and attention trainers
status: open
priority: 3
issue-type: task
created-at: "2026-07-20T23:45:52.311005+02:00"
---

Unowned gap surfaced by the inventory rebuild (03834011): cosine LR schedule and global-norm gradient clipping are wired only into the Adam-MLP trainer; the SGD trainer and the attention/block trainers take neither. Extend both trainer families to consume the same schedule + clip machinery, with locked run-twice proofs and a red-first assert that the schedule actually modulates the step (loss trajectory differs from constant-LR base).
