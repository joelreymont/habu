---
title: "Use x86's short immediates in T0"
status: open
priority: 2
issue-type: task
created-at: "2026-11-25T15:18:20.273368+01:00"
---

Problem: t0-x64.f emits imm32 ALU operations, loads zero with a 5-byte mov and pushes literals through rax. Acceptance: imm8 ALU forms, test for a compare with zero, xor for zero when the condition codes are free, and literals stored directly; the engine converges on pulsar and its tests are green. Depends: none. Claim: agent=jerry workspace=.jj-ws.noindex/jerry-x-imm
