---
title: "EPIC: operational Habu change-file milestones"
status: open
priority: 1
issue-type: task
created-at: "2026-07-17T13:03:09.467503+02:00"
---

Task anchor: keep master operational while finishing the unified ENUM/STRUCTURE hard cutover and then the safe change-file pipeline. Execute milestones strictly in order; no implementation or merge from milestone N+1 starts before milestone N is independently reviewed, fully gated on its exact rebased tree, fast-forwarded to master, pushed, and closed. Every milestone owns its dots, one integration proof, and cleanup of obsolete workspaces. Final outcome: checked no-follow and atomic FS primitives, authenticated modular compilation with canonical cache, one bulk content scan, one complete framed change artifact, green full gates, and all associated dots closed.
