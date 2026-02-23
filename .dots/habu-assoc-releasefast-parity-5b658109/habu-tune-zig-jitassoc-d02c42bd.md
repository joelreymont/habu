---
title: Tune Zig jitAssoc loop shape
status: closed
priority: 2
issue-type: task
created-at: "\"\\\"2026-02-23T09:23:20.874178+01:00\\\"\""
closed-at: "2026-02-23T09:26:27.793038+01:00"
close-reason: "Rejected: loop-shape tweak regressed ReleaseFast assoc"
---

src/jit/backend.zig jitAssoc: simplify loop control and remove redundant masking in pointer loads while preserving cons-tag guards; rebench ReleaseFast assoc and keep only wins.
