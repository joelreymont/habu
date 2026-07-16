---
title: Retire PTY group after leader exit
status: active
priority: 2
issue-type: task
created-at: "\"2026-07-16T14:19:50.477630+02:00\""
---

Full context: lib/process-pty-io.f SUP-TARGET-DIED reaps and reports the leader, then the supervisor exits without killing remaining same-process-group descendants. The session owns a target process group, so WAIT must not retire the only authority while group members survive. Fix supervisor target-exit handling to preserve leader status, kill the immutable process group, preserve cleanup evidence, and add a checked regression whose leader exits before its same-group child and whose CLOEXEC sentinel proves the child is gone. Run PTY focused tests, typed diff, trust, host, filemap, candidate phase16, and full cold gate.
