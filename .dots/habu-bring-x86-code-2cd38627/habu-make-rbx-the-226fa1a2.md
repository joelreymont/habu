---
title: Make rbx the x86 data stack register
status: closed
priority: 2
issue-type: task
created-at: "2026-11-25T15:18:20.251447+01:00"
closed-at: "2026-11-25T17:17:42.061731+01:00"
---

Problem: r12 as the data stack register costs a SIB byte on each of 237,857 accesses; rbx is free (X64-INTERP has no reader). Acceptance: rbx is the x86 data stack, r12 holds the innermost loop index as arm64's x29 does, the engine converges on pulsar and its tests are green. Depends: none. Claim: agent=jerry workspace=.jj-ws.noindex/jerry-x-dsp
