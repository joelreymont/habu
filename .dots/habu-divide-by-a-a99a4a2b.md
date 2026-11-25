---
title: Divide by a literal without a divide
status: active
priority: 2
issue-type: task
created-at: "2026-11-25T19:34:25.734097+01:00"
---

Problem: src/habu/t0-x64.f DIVIDE and t0-arm64.f DIV-LIT and MOD-LIT compile a literal divisor to idiv and sdiv; code-bench's branch kernel (2 mod) takes 4558 us on pulsar and 3536 us on spark, the one kernel x86 loses. Acceptance: the kernel is no slower on pulsar than on spark, every literal divisor answers as the kernel's rows, and neither engine grows by more than a page. Depends: none. Claim: agent=worker workspace=.jj-ws.noindex/jerry-div.
