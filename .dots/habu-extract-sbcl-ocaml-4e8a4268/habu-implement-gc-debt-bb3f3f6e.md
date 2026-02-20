---
title: Implement GC debt trigger model
status: open
priority: 1
issue-type: task
created-at: "2026-02-20T08:55:19.472084+01:00"
blocks:
  - habu-optimize-remembered-set-4ebdf466
---

File: src/runtime/gc.zig:1; cause: current trigger policy is simplistic and spike-prone; fix: debt-based major trigger with target pause budget; why: smoother latency like SBCL/OCaml.
