---
title: "Classify the library containers' own checks"
status: active
priority: 2
issue-type: task
created-at: "2026-11-23T09:30:30.798113+01:00"
---

Problem: src/lib/array.f A-CHECK-INDEX (28, 48-70), src/lib/c2-memory.f CHECK-INDEX (330-417), src/lib/queue.f Q-CHECK-CAP (90, 253) and src/lib/byte-buffer.f CHECK-CAP (52-150) check indexes and capacities at run time, like MAP's checks before their removal. Acceptance: each is deleted where it catches a program error, or named as guarding outside data; their suites pass. Depends: none. byte-buffer.f goes with habu-buf-buf-a-48c31f14. Claim: agent=jerry workspace=.jj-ws.noindex/jerry-p5a (array), .jj-ws.noindex/jerry-p5c (c2-memory), .jj-ws.noindex/jerry-p5q (queue).
