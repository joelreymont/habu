---
title: Find the pty wait that expires under load
status: open
priority: 2
issue-type: task
created-at: "2026-10-31T18:36:10.811755+02:00"
---

Problem: in lane t0tr's full suite (load average ~100, /tmp/jerry-big/t0tr-suite.log lines 2297-2330) row engine-runtime-regressions failed in case process/pty: test/proc-pty.f assert 124 'expected true got false', its buffer showing '99 throw' answered by '?'. The same row passed alone on the candidate and on master's engine (2/2 each) and rc 0 on rerun. Acceptance: name the wait in test/proc-pty.f that expired or the output it raced, and make the case wait on the event with a deadline, failing only on a real fault. Files: test/proc-pty.f. Verify: the row under a loaded host. Depends: none. Ownership: test/proc-pty.f. Claim: unassigned.
