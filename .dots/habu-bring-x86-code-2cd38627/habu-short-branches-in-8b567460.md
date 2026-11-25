---
title: Short branches in the x86 kernel
status: open
priority: 2
issue-type: task
created-at: "2026-11-25T15:41:05.669495+01:00"
---

Problem: src/habu/kernel-x64.f's own words take rel32 for branches that fit rel8 (1,290 jcc and 423 jmp in map rows with no source file, about 6.4 KB). Acceptance: each such branch takes its short form, the engine converges on pulsar, and arm64 output is byte-identical. Depends: the short and dsp lanes. Claim: unassigned.
