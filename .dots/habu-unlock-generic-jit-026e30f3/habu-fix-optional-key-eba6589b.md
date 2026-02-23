---
title: Fix &optional + &key positional semantics
status: closed
priority: 1
issue-type: task
created-at: "\"\\\"2026-02-23T19:07:59.682722+01:00\\\"\""
closed-at: "2026-02-23T19:33:42.506488+01:00"
close-reason: completed in c93d2555
---

src/interp/vm.zig:10951 currently scans for first keyword inside optional slots; this misclassifies positional keyword values and breaks ANSI read-from-string feature-plus tests. Align doCall argument partitioning with CL behavior (consume optional slots positionally up to max_positional; keyword pairs begin only after optionals). Update integration coverage at src/tests/integration.zig:2400 to lock semantics.
