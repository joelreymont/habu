---
title: Run x86 exit actions as arm64 does
status: active
priority: 2
issue-type: task
created-at: "2026-11-17T10:44:56.771119+01:00"
---

Problem: x86 has no twin of EMIT-EXITHOOK: a task's die and an uncaught throw run no exit actions (exit-hook red on x86). Acceptance: exit-hook passes on x86. Depends: none. Claim: agent=carl workspace=.jj-ws.noindex/carl-x86 (merged, lands with the feature).
