---
title: "Take a fault at the signal stack's end as Linux does"
status: active
priority: 1
issue-type: task
created-at: "2026-11-25T16:15:24.640964+01:00"
---

Problem: a fault whose stack pointer equals the signal stack's base counts as on that stack (src/habu/boot-x64.f ON-SIGNAL-STACK, and src/habu/crash.f C-CRASH-ONSTACK>R9 test sp - ss_sp < size; Linux tests ss_sp < sp <= ss_sp + size), so the crash handler dumps instead of reporting the error's record. Acceptance: both handlers test as Linux does, and the x86 images kernel-crash-zero-base, skel, skel-negative and skel-underflow end as their tests document. Depends: none. Claim: agent=worker workspace=.jj-ws.noindex/jerry-x-skel.
