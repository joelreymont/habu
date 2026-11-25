---
title: Short forms for x86 frames and branches
status: closed
priority: 2
issue-type: task
created-at: "2026-11-25T15:38:48.805434+01:00"
closed-at: "2026-11-25T17:17:42.131533+01:00"
---

Problem: x86-64 T0 code uses rel32 branches, empty frames and imm32 frames (about 370 KB of the 752 KB gap to arm64). Acceptance: the engine converges on pulsar, arm64 output is byte-identical, code size and code-bench are measured before and after. Depends: the place and imm lanes. Claim: agent=worker workspace=.jj-ws.noindex/jerry-x-short.
