---
title: Spawn through a SPAWN request
status: open
priority: 1
issue-type: task
created-at: "2026-10-28T20:27:02.223612+02:00"
blocks:
  - habu-add-spawn-fds-2460a49d
---

Fork-to-exec lane 1b. Joel 2026-10-28 (interview) approved: package SPAWN with SPAWN:NEW ( path -- SPAWN:request ), SPAWN:ARGV, SPAWN:ENV, SPAWN:CWD, SPAWN:FD ( req child parent -- req ), SPAWN:CLOSE; a new request inherits fds 0-2 and closes the rest; PROCESS:SPAWN ( SPAWN:request -- pid ) runs it over spawn-fds. The 17 PROC-SPAWN-* words and the PROC-CWD spawn words are rebuilt over it here. Claim: carl.
