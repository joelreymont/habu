---
title: "Undo AIO:START when its loop task fails to start"
status: active
priority: 2
issue-type: task
created-at: "2026-11-06T13:26:16.389709+02:00"
---

Problem (tim, Maki, 2026-11-06; /Users/joel/Work/maki/.jj-ws.noindex/tim/.out/audit/habu-aio-start.md): src/lib/aio.f:1330-1338 START runs HOST-SETUP, sets RING-LIVE 1, then TASK:ACTIVATE. When ACTIVATE throws (E-TASK-THREAD from pthread_create, src/lib/task.f:1583-1584) nothing undoes the setup: RING-LIVE stays 1, the host's resources stay held (macOS MAC-SETUP's pipe and polls buffer, aio-macos.f:285-287; the Linux ring), the next START throws E-AIO-STATE, RUNNING? answers true for a loop that never ran, and STOP would join a task never activated. Maki's SERVICE:START relies on a START that throws leaving nothing for STOP, as HTTP:START guarantees (src/lib/net/http.f:932-944, ROLLBACK-START). Acceptance: a START whose ACTIVATE throws runs HOST-CLOSE, leaves RING-LIVE 0 and rethrows; a following START succeeds; a regression test that makes ACTIVATE fail and then starts again, failing on current code. Related, not the same: habu-ring-setup-leaks-b7a7dc29. Files: src/lib/aio.f, its test (src/lib/aio-test.f or the AIO test rg finds). Verify: the AIO test rc 0. Depends: none. Ownership: carl, worker lane. Claim: agent=carl workspace=.jj-ws.noindex/carl-aio.
