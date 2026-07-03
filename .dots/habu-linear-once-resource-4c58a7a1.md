---
title: Linear-once resource capability
status: open
priority: 2
issue-type: task
created-at: "2026-07-01T22:54:40.850475+02:00"
---

Checker capability for state-frame boundaries currently TRUSTED: evaluate/include frames (EVAL-FRAME $3800..$39FF overlap class - LESSONS.md:74-79), mmap slots, snapshot phases. Design: linear/once witnesses - a resource role produced exactly once and consumed exactly once (DEFLINEAR exists at src/core/roles.f:80-82 - extend to enforcement in the checker rather than convention), so acquire/release pairing is proven, discharging the trusted wrappers around frame push/pop. Also gives once-space witnesses the AD design wants (docs/maki/autograd.md:49-57). Design doc + capability + migrate include.f INCLUDE-PUSH/POP as the worked example.
