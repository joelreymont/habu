---
title: Fix slot-exists-p
status: open
priority: 1
issue-type: task
created-at: "2026-02-01T22:29:42.092792+01:00"
---

Context: src/runtime/primitives/clos.zig:247; cause: non-vector returns t; fix: return nil (or InvalidArgument) and align with slotBoundp; deps: none; verification: add/adjust clos.zig tests
