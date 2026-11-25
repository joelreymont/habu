---
title: Prune the kernel from x86 programs
status: active
priority: 2
issue-type: task
created-at: "2026-11-17T10:48:16.820508+01:00"
---

Problem: NATIVE:CUT (src/habu/link-kernel.f:689) returns at once on x86, so a stripped x86 program keeps the whole kernel, about 66 KB of it unreached in tenderd. Acceptance: a stripped x86 program links only the kernel words it reaches, as on arm64. Depends: none. Claim: agent=worker workspace=.jj-ws.noindex/jerry-x-kcut.
