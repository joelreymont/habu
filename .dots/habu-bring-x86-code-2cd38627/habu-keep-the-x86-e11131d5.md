---
title: Keep the x86 loop index in r12
status: open
priority: 2
issue-type: task
created-at: "2026-11-25T15:38:48.835794+01:00"
---

Problem: x86-64 T0 keeps a loop's index in memory, where arm64 keeps it in a held register (about 238 KB of SIB-byte addressing is tied to r12 as the data stack register). Acceptance: the loop index lives in r12 on arm64's HELD scheme, the engine converges on pulsar, arm64 output is byte-identical and code-bench is measured before and after. Depends: the dsp and short lanes. Claim: unassigned.
