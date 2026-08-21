---
title: Build native loop SSA
status: closed
priority: 1
issue-type: task
created-at: "\"2026-07-26T22:59:19.987529+02:00\""
closed-at: "2026-08-14T11:51:17.251263+02:00"
close-reason: "Closed SATISFIED (Wave-3 audit 2026-08-14): all five loop forms + RECURSE with explicit back edges and loop-carried block arguments; zero-trip/back-edge/early-exit/nested/recursion all registered differentials in both gate surfaces plus live probes; carried-value mutations reject E-NELAB-JOIN with named codes. Residues owned: dead-latch 675222be, unstructured exit 7e013b93. SIR naming drift as on the sibling."
---

Full context: design Wave 3 adds BEGIN/UNTIL/AGAIN/WHILE/REPEAT and RECURSE with explicit back edges and loop-carried block arguments. Acceptance: zero-trip, back-edge, nested-loop, early exit, recursion, dominance, and carried-value mutations pass or reject with named diagnostics.
