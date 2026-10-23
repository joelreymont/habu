---
title: Fix fs-mutate REMOVE-TREE socket flake
status: open
priority: 2
issue-type: task
created-at: "2026-10-23T17:33:29.557618+03:00"
---

Problem: fs-mutate's 'REMOVE-TREE unlinks a bound Unix socket and a nested FIFO' failed once on macOS arm64 with -2105 ('staged replacement cleanup threw -2105', 7.39 s) and passed on two reruns (g5 test audit lane, host load ~20).
Acceptance: the root cause is found and fixed at its layer; the case passes 50 consecutive runs under load.
Verification: repeated bin/hb --load runs of the fs-mutate suite.
