---
title: Run the cleanup registry at process exit
status: open
priority: 3
issue-type: task
created-at: "2026-09-22T15:28:21.218611+03:00"
---

Problem: lib/fs-mutate.f CLEANUP+/CLEANUP-DIR+/CLEANUP-TREE+ fill a table only CLEANUP-RUN walks; a die, an uncaught top-level throw or a T-REPORT that dies leave every registered path behind (aspen's reproducer ~/.cache/tender/habu-gaps/cleanup-at-exit/leak.f: TMPDIR-MKDIR, CLEANUP-TREE+, 1 throw; the tree stays). The engine has no process-exit hook: IMAGE-LIFECYCLE:REGISTER is the image-prepare chain and TASK:AT-EXIT is per task. Acceptance: one fixed exit vector cell the engine calls once before exit_group on the normal top-level exit (EX0), in die (BDIE) and in the uncaught top-level throw handler (LUNCAUGHT), cleared before the call so a hook that dies cannot recurse, with the exit code preserved; native and mirror (bootstrap/cg/forth.fs); lib/fs-mutate.f registers CLEANUP-RUN on it when loaded; leak.f leaves nothing; a fixture pins the three paths and that a hook which throws still exits with the original code. Files: src/habu/habu2.f, bootstrap/cg/forth.fs, lib/fs-mutate.f, test/. Verify: the fixture; three generations; the recovery check. Depends: none. Ownership: hazel. Claim: unassigned.
