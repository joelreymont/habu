---
title: "Share the rejection stacks' growth"
status: closed
priority: 2
issue-type: task
created-at: "2026-11-15T07:48:19.986311+01:00"
closed-at: "2026-11-16T04:07:34.055568+01:00"
---

Problem: src/core/rejection.f's label stack (PRESERVE-LABEL) copied HELD-ROOM's growth code and was named KEPT-*, beside the public REJECTION:KEPT. Acceptance: one growth word serves both stacks; the label stack's names do not echo KEPT or SAVED$. Depends: none. Claim: agent=jerry workspace=.jj-ws.noindex/jerry-c4m2 (in the integration stack, closes at landing).
