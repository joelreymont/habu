---
title: Maxima end-to-end integration test
status: active
priority: 2
issue-type: task
created-at: "\"\\\"\\\\\\\"\\\\\\\\\\\\\\\"\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\"\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\"\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\"2026-02-17T10:36:45.704609+01:00\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\"\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\"\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\"\\\\\\\\\\\\\\\"\\\\\\\"\\\"\""
---

src/tests/integration.zig: Add integration test that loads Maxima stubs + core files and verifies: simplifya(3+4)=7, diff(x^2,x)=2x, solve(x^2-4,x), integrate(x^2,x), factor(x^2-1), limit(1/x,x,0), determinant([[1,2],[3,4]]), expand((x+1)^3), sin(0)=0, cos(0)=1. Requires heap ~48MB for test. Depends on: habu-fix-do-cond-9fbc7d1f, habu-increase-default-heap-44a06bce. Files: /tmp/maxima/ has Maxima source. Est: 3h
