---
title: Spawn through a SPAWN request
status: open
priority: 1
issue-type: task
created-at: "2026-10-28T20:27:02.223612+02:00"
blocks:
  - habu-add-spawn-fds-2460a49d
---

Fork-to-exec lane 1b. Joel 2026-10-28 (interview) approved: package SPAWN with SPAWN:NEW ( path -- SPAWN:request ), SPAWN:ARGV, SPAWN:ENV, SPAWN:CWD, SPAWN:FD ( req child parent -- req ), SPAWN:CLOSE; a new request inherits fds 0-2 and closes the rest; PROCESS:SPAWN ( SPAWN:request -- pid ) runs it over spawn-fds. The 17 PROC-SPAWN-* words and the PROC-CWD spawn words are rebuilt over it here. Joel 2026-10-28 (interview) approved landing phil's process argv/env growth lane first (growable never-moving PROC-CHUNKS, PROC-ARGV-ROWS, PROC-ENV-ROWS); the request builds its storage on those chunks. phil's hazard: PROC-ENV-DEFAULT-RESET run between a CMD PREPARE and its spawn rewinds the default chunks under the prepared command; a request must own the storage it hands spawn-fds. Claim: carl.

Joel 2026-10-29 (interview) approved two more public words: SPAWN:SPAWN, exported under PROCESS as PROCESS:SPAWN (PROCESS cannot erase SPAWN's linear token), and SPAWN:ERRNO ( -- n ), the task-local positive errno of the last refused spawn. Design: carl-forkexec/.out/design-1b.md (Plan, Opus), with lead rulings R1 (a default 0-2 slot whose parent fd is closed becomes -1) and R2 (RAW effects unchanged). Two commits: C1 process-chunks.f, spawn.f, spawn-test.f; C2 RAW rebuild.

1b C2 fold-in 2 (carl-forkexec .out/brief-1b-c2-fold2.md): capture-tree, check-signal, gate-signal and pg-kill observe tree death through a FIFO that test code opens by path and maps explicitly. Library spawn words carry no extra fds; the capture and pool words are unchanged.

Joel 2026-10-29: superseded by the approved PROCESS API (parent dot): package SPAWN becomes PROCESS, request PROCESS:request, CLOSE becomes CLOSED, plus ARG/ENV/HERMETIC/STDIN/CANCEL/TICK/RUN; this lane is PROCESS lane 1.
