---
title: Close the pre-fork window in the tree kill
status: open
priority: 3
issue-type: task
created-at: "2026-09-30T18:33:01.944305+02:00"
---

Problem: lib/process-tree.f (change monlstxr) freezes a tree with SIGSTOP and counts a member settled when it is stopped with no runnable thread (pti_numrunning 0). On macOS a thread blocked in a kernel wait inside posix_spawn before the child is inserted in the process list (page-in of the spawn descriptors, zone allocation, waiting for proc_list_lock, which the walk's own libproc calls take) reads as settled; the SIGKILL that follows lets it finish fork and exec, and the child, which leads its own group (POSIX_SPAWN_SETPGROUP, src/habu/habu1.f:1107) and is reparented to launchd, escapes the kill. Found by the review of monlstxr from XNU's spawn path; not observed (0 escapes in 36 stress runs after the fix). The window is microseconds unless paging, and both settle passes must land in it. Acceptance: either a mechanism that makes the escape impossible by structure (for example a kernel-tracked grouping every descendant inherits and cannot leave, or a post-kill sweep that identifies escaped descendants by something they cannot shed), with an E2E case that holds a member inside a spawn and shows no descendant survives; or a measured statement, in docs/gate.md, of why macOS offers no such mechanism and what bounds the window. Linux: say whether the /proc arm has the same window (a task in clone before the child is visible). Files: lib/process-tree.f, test/gate-signal-test.f, docs/gate.md. Depends: habu-end-a-signalled-14f5465b. Ownership: the tree kill's completeness. Claim: unassigned.
