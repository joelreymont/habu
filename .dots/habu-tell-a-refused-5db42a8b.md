---
title: Tell a refused /proc open from an ended process
status: open
priority: 2
issue-type: task
created-at: "2026-11-18T00:20:39.636940+01:00"
---

Problem: src/lib/process-tree.f takes every stat file or task directory that will not open (open-rd at :277 and :530, the comment above them) as a process or thread that ended, a refusal such as EACCES under hidepid included; the engine's open stores errno (FS:ERRNO), so the two can be told apart, and a refused member is now counted as gone. Acceptance: an open that fails with ENOENT or ESRCH is an ended member; a member whose open fails with any other errno is still signalled with the tree, never dropped as ended, and the walk still ends; a test pins the ended case. Depends: none. Claim: unassigned.
