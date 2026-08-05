---
title: Unpin the emit byte-size fixture
status: open
priority: 2
issue-type: task
created-at: "2026-08-05T10:29:50.026106+02:00"
---

CG-06, tip red. tools/codegen-workload-test.f:298-301 expects production word 'emit' to be exactly 44 bytes; it is 52 on the integrated Linux tree, failing F23. Changing 44 to 52 merely moves the magic value. Fix: if the inline boundary being probed is still useful, define a test-owned word of controlled size exactly one instruction over the boundary so the fixture derives its own expectation; otherwise delete the assertion. Per the derive-pinned-facts rule: never hand-assert what the build can measure.
