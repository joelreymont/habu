---
title: "Make the process package's poll storage task-local"
status: open
priority: 2
issue-type: task
created-at: "2026-09-17T11:25:59.986667+03:00"
---

Problem: lib/process.f keeps one pollfd array (PROC-PFD) and its capture slots for the whole process while lib/net/tcp4.f and udp4.f keep theirs per task through TASK:+USER, so a task in PROC-CMD:RUN-OUTCOME capturing a child and another task calling POLL-IN on a pipe overwrite each other's slot 0 (found by the Tender scheduler lane, 2026-09-17, which had to wait on a loopback UDP socket instead of POLL-IN); docs/threads.md's storage-class table says process-wide for process. Acceptance: the poll array and per-call capture slots live in the task user band (USER-BAND after habu-give-the-task-57a9243e lands), the module header and docs/threads.md say task-local, and a test runs one task capturing a child through RUN-OUTCOME while another polls a pipe through POLL-IN and both complete correctly. Files: lib/process.f, lib/process-*.f, their tests, docs/threads.md. Verify: the process suites, test/run.f. Depends: habu-give-the-task-57a9243e. Ownership: lib/process*. Claim: unassigned.
