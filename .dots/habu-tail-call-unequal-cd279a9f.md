---
title: Tail-call unequal native argument rows
status: active
priority: 2
issue-type: task
created-at: "2026-10-16T08:36:57.133909+03:00"
---

Native tail qualification currently requires caller input, callee input and result counts to match. This blocks ordinary DYNAMIC-BUFFER accessor/reserve/release wrappers, including zero-input release. Carry actual bound callee input/result counts through the tail operation, preserve lower stack cells, place callee arguments above caller base, set SP to base+callee inputs, restore frame/link, branch, and require callee results equal caller results. Keep terminal straight-line/dead-carried restrictions and exact bound target/memory token. Revise existing native-tail E2E before code for unequal and zero inputs, retained xt and lower-stack sentinel, and emitted branch/no trailing return; run focused source compiler tests with matching clean engine. Combined rebuild, full native gate, convergence, review and push belong to integrator.
