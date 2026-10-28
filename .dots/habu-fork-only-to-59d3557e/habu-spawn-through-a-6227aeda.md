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
