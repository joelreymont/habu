---
title: Bound the x86 profiler rate as arm64 does
status: active
priority: 2
issue-type: task
created-at: "2026-11-17T10:48:16.710376+01:00"
---

Problem: x86 RATE-GUARD, accepts a rate above OS-TIMER-MAX-USEC that arm64 BPROF-RATE rejects (native-gate-debug red on x86). Acceptance: native-gate-debug passes on x86. Depends: none. Claim: agent=carl workspace=.jj-ws.noindex/carl-x86 (merged, lands with the feature).
