---
title: Name the primitive in every kernel task refusal
status: open
priority: 2
issue-type: task
created-at: "2026-11-12T14:02:21.838232+02:00"
---

Problem: B-TASK-GUARD (src/habu/habu1.f:1521-1524) exits 79 with nothing on fd 2, and habu2.f's DEFWRITE primitives exit the same way, so a compile on a task's thread names neither the primitive nor the cause. Acceptance: each B-TASK-GUARD caller passes its primitive's name and the guard writes it through the kernel emitter; the DEFWRITE primitives do the same; docs/forth-card.md's exit-79 paragraph and docs/debugging.md say what is printed. Depends: the kernel emitter dot. Claim: unassigned.
