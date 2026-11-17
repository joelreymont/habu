---
title: "Take VEC:ENSURE's fast path before its capacity check"
status: open
priority: 2
issue-type: task
created-at: "2026-11-17T13:15:02.114658+01:00"
---

Problem: VEC:ENSURE and VEC:RESERVE (src/lib/vector.f:97, :104) run CHECK-COUNT (:39), whose MOST divides, before the test that ends ENSURE when the count already covers n, so every in-range access pays a division: 7.4 s self plus 0.7 s of / in a 123 s Maki STEP import, most of it through STORAGE:RECORD-CELL (Maki hunt imptime, 2026-11-17, sampling profile /tmp/tim-imptime/prof1.json). Acceptance: ENSURE of an n no larger than COUNT, and RESERVE of one no larger than the room, divide nothing and keep their E-VEC-CAPACITY rejections. Depends: none. Claim: unassigned.
