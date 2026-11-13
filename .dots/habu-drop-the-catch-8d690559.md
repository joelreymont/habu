---
title: "Drop the catch frame's stack restore if no switch needs it"
status: open
priority: 2
issue-type: task
created-at: "2026-11-13T21:08:45.619108+01:00"
---

Problem: a catch frame saves the active data-stack base and capacity (src/habu/habu1.f:3044-3045 BCATCH-RUN, :3305-3306 a loaded file's frame) and a throw restores them (habu1.f:3069-3072 BTHROW-UNWIND, src/habu/habu2.f:2641-2644 delivery); they were added to carry the allocation through run-in-stack. With it deleted, every remaining switch restores its caller's allocation before a throw leaves it (a closed text's CLOSED-LEAVE, habu1.f:3224, on its throw path at :3260; a loaded file at :3322-3327; a task), so the restore may only rewrite the value already in force. The evaluate frame's own saved pair (habu2.f:2568-2570, pushed at src/habu/interpreter.f:2662-2663) is in the same position. The checker resync armed at habu2.f:2614 runs at :2625-2627, before the restore. Acceptance: either a program whose catch delivery finds a base or capacity other than the frame's, kept as a test and named in the restore's comment, or the saved cells and the restore are deleted and the suite passes. Depends: none. Claim: unassigned.
