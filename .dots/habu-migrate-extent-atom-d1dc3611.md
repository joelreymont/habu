---
title: Migrate extent-* atom sigs to TFAM extents
status: open
priority: 3
issue-type: task
created-at: "2026-07-18T18:23:51.009926+02:00"
blocks:
  - habu-extent-typed-tensor-bde435dc
---

Consistency migration decided in docs/extent-substrate.md Open Question 1: adopt TFAM extents as the single substrate and migrate the live extent-* atom sigs - maki/fusion.f:71 (production), maki/eval/repair-mech-test.f:85,87 and maki/eval/device-fault-test.f:28,32 (checked eval fixtures, span<...,extent-n>) - plus docs/ptx.md examples, onto declared TFAM extent families. Device families accept TFAM extent args transparently (proven in test/extent-substrate-probe.f + the destruction review span/matrix probes), so this is representation-neutral. Atoms and TFAM extents may coexist during the transition (both unify by identity); the target is one substrate. After migration, consider retiring the extent- atom prefix from the parser prefix list (checker.f:2478) with a negative regression. Blocked by EXTENT:/TENSOR: existing (habu-extent-typed-tensor-bde435dc).
