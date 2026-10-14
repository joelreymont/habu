---
title: Make directory listing task-safe
status: closed
priority: 2
issue-type: task
created-at: "\"2026-10-02T23:12:52.724548+02:00\""
closed-at: "2026-10-14T19:43:43.277194+03:00"
close-reason: "Directory listing and walk lock made task-safe in d7d3ba72; current process-fork.f registers PROC-TREE:CHILD-RESET. Remaining other-lock issue has separate af5c91a3."
---

Problem (lane 361 r4-killtree): src/lib/fs-list.f's listing buffers are process-wide (create BLOCK, ~:22) and src/lib/process-tree.f's Linux arm lists /proc through it; a task listing a directory while another task's capture ends early (KILL-TREE walk) corrupts both listings. Also a process forked while a task holds process-tree's WALKING flag inherits it held (src/lib/pg.f and src/lib/task.f spin locks share the exposure). Acceptance: FS-LIST state per call or per task (or the /proc walk reads with its own buffer), a two-task test listing and walking concurrently gets both results whole; a forked child resets held process-wide locks it cannot own (ENTER-CHILD), shown by a fork under a held WALKING. Files: src/lib/fs-list.f, src/lib/process-tree.f, src/lib/process-fork.f.
