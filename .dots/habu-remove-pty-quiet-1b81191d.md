---
title: Remove PTY quiet tax
status: active
priority: 2
issue-type: task
created-at: "\"2026-07-13T04:49:18.298003+02:00\""
---

Full context: lib/process-pty.f DRAIN polls 10 ms through five quiet polls, imposing at least 50 ms on every synchronization and multiplying full-gate time. Use prompt/sentinel-driven EXPECT? for command completion and a zero/one-wait opportunistic drain; add latency and poll-count ratchets. Group with active PTY hardening dots.
