---
title: Design spawn-based isolation for tests
status: closed
priority: 1
issue-type: task
created-at: "2026-10-28T19:04:35.640625+02:00"
closed-at: "2026-10-28T19:54:55.065030+02:00"
close-reason: "Design (Fable) with Joel's rulings of 2026-10-28 (interview). Snippets: SUBJECT becomes SNIPPET (see the rename dot); the child program src/lib/test/snippet-run.f reads fd 0 to EOF and evaluates it closed, under run-in-stack on a guarded page; libraries come in through SNIPPET:REQUIRE, and test-only words move to test/<name>-prelude.f. Rollback in one process is rejected: it restores only the dictionary, and die, crash and timeout cases need a process anyway. Spawn: one engine primitive, spawn-fds, over a request record (path, argv, envp, cwd, fd map); the child's fd table is exactly the map. Linux uses close_range with CLOSE_RANGE_CLOEXEC (kernel 5.11 floor) plus dup2 and execve; macOS uses POSIX_SPAWN_CLOEXEC_DEFAULT plus a dup2 action per slot. Public: PROCESS:SPAWN ( SPAWN:request -- pid ) over the request record (Joel); the 17 combinatorial PROC-SPAWN-* words are deleted and their callers fill a request (Joel); CLOSE-EXCEPT2, MAXFD, IO-CLOSE-HIGH and the four stdio spawn primitives go. Reaper: one exec'd helper per pool (src/lib/process-reaper.f), fed +pid/-pid lines on fd 3, KILL-TREE on every live pid at EOF; words PROCESS:REAPER-START ( -- PROCESS:reaper ), PROCESS:REAPER-WATCH and PROCESS:REAPER-FORGET ( reaper pid -- ), PROCESS:REAPER-STOP ( reaper -- ), in PROCESS rather than their own package, since a reaper only kills process trees. DEATH-PIPE, FORK-REAPER, SPAWN-REAPER, REAP-WATCH-FD and PROC-REAP-ARM are deleted. Other callers: ROW-RED reports through the slot; GE-EVAL-FORK becomes SNIPPET:RUN; prop-test MAIN takes -- shard / -- sweep; the live-verify writer becomes its own file; boot-census fixture child is /bin/sh; the PTY target uses spawn-fds, with IO-WATCH treating ESRCH as exited; fork-only tests are deleted, and one pool-death test replaces gate-pool-orphan-test. Lanes: 1 spawn-fds + PROCESS:SPAWN (field words go to Joel), 2 SNIPPET, 3 users, 4 reaper, 5 other callers + PTY, 6 deletion (fork-child.f, process-fork.f, fork primitive, ACROSS-FORK hooks). Open: kqueue on a zombie, prop-test load per shard."
blocks:
  - habu-census-and-measure-a8a20e75
---

Feature design (Plan, Fable) from the census: how a test case, a gate-pool worker, an eval row and a prop shard run in a fresh hb; what replaces the inherited definitions; process-tree and reaper without fork-and-continue; deletion list. Claim: unassigned.
