---
title: Build native loop SSA
status: open
priority: 1
issue-type: task
created-at: "2026-07-26T22:59:19.987529+02:00"
blocks:
  - habu-build-native-branch-1a7087bd
---

Full context: design Wave 3 adds BEGIN/UNTIL/AGAIN/WHILE/REPEAT and RECURSE with explicit back edges and loop-carried block arguments. Acceptance: zero-trip, back-edge, nested-loop, early exit, recursion, dominance, and carried-value mutations pass or reject with named diagnostics.
