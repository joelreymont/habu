---
title: Fold a constant tree instead of moving it
status: open
priority: 2
issue-type: task
created-at: "2026-08-13T00:31:44.580146+02:00"
---

Boundary from the hoist landing (001b2e90): a value built out of nothing but constants is FOLDED as a single addend, never MOVED - principled (this pass folds numbers) but also load-bearing: moving a large constant tree makes the rewritten module hit E-IR-CTX-SCRATCH at ~27 constants, turning a compiling program into a refusing one. The missing capability is folding a constant TREE (constant propagation over the invariant ops), not moving it. Interacts with the scratch ceiling (82b7ceb2/59aa92b7). Files: src/compiler/native/loop.f. Depends: none.
