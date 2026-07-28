---
title: Per-context function-list stage state
status: open
priority: 2
issue-type: task
created-at: "2026-07-28T20:56:48.037711+02:00"
---

Full context: IR-TYPE's function-type staging area (FN-BEGIN/FN-PARAM/FN-RESULT consumed by QUOT/CODE-REF) is package-owned process-wide state, following the sanctioned CTARGET/IR-CTX staging precedent, because Habu cannot pass variable-length id lists on the stack. Same single-task debt class as the IR-CTX and IR-ARENA registries: before concurrent compilation, move the stage into per-context state (or guard entry with task ownership) and add a concurrency witness. Batch with habu-per-task-compiler-812638a6.
