---
title: Run the forked eval child under CHILD-MAIN
status: open
priority: 2
issue-type: task
created-at: "2026-11-12T15:53:44.102741+02:00"
---

Problem: test/gate-common-lib.f GE-EVAL-FORK-CHILD (:407-410) is not a PROC-FORK:CHILD-MAIN root, so a throw from GE-EVAL-REDIRECT! (:408) resumes the parent's copied catch frames in the child (src/lib/process-fork.f:94-96) and the child runs parent code. Acceptance: the child runs under CHILD-MAIN and a throw there ends the child with its code. Moving GE-EVAL-FORK-SPAWN onto PROCESS:CAPTURE-ADOPT-SPAWN is not part of it: the adopt word stores the pid through CAPTURE-PID!, which arms a reaper the raw store skips. Depends: none. Claim: unassigned.
