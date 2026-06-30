---
title: Forked resident test workers
status: open
priority: 2
issue-type: task
created-at: "2026-06-30T21:29:45.151283+02:00"
blocks:
  - habu-split-gate-runner-59bb0aaf
---

Problem: resident in-process tests eliminate hb exec/recompile cost but serialize work and share unsafe compiler/checker globals; threads are unsafe for dictionary/checker/process argv state. Fix: add a typed fork/process-worker primitive and test-pool path: parent loads suite support once, forks copy-on-write child workers for semantic test groups, children execute checked dispatch words and exit, parent captures stdout/stderr/status through pipes and schedules them with the existing pool semantics. Linux: expose clone(SIGCHLD,0,0,0,0) as fork-like primitive; macOS: expose direct fork syscall. Keep exec/spawn only for true CLI/candidate/process-boundary tests. Acceptance: fork smoke passes on macOS and zed/Linux, child dictionary mutations do not affect parent, child failure reports deterministic status/output, resident semantic tests run in parallel without broad warm runner image build, full Mac hot/cold <=30s target or gives measured remaining long pole.
