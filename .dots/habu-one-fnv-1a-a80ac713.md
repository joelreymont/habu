---
title: Name the FNV-1a pair in the x86-64 kernel
status: open
priority: 2
issue-type: task
created-at: "2026-11-16T16:26:06.292148+01:00"
---

Problem: src/habu/kernel-x64.f (FNV-BASIS, FNV-PRIME, used by HASH,) still names its own FNV-1a constant pair, where every other site names src/habu/fnv.f's. Acceptance: kernel-x64.f names FNV:BASIS and FNV:PRIME, requires src/habu/fnv.f, and defines no pair of its own. Depends: the x86-64 step (no x86 build until then). Claim: unassigned.
