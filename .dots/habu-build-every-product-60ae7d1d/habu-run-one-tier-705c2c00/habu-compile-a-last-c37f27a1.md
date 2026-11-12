---
title: Compile a last-call recurse as a tail call in T0 and tier 1
status: open
priority: 2
issue-type: task
created-at: "2026-10-30T19:23:10.793294+02:00"
---

Problem: T0 and tier 1 compile no last-call recurse as a tail call (T0's DO-RECURSE, src/habu/t0.f:1064, always calls); a word recursing 2000000 deep dies 'stack bounds exceeded (machine)' rc 102 where the JIT's last-call B ran. Acceptance: native tail call; a test recursing that deep passes on T0. Claim: unassigned.
