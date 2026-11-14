---
title: Reuse a part whose root it never reads
status: open
priority: 2
issue-type: task
created-at: "2026-11-13T06:35:53.967998+02:00"
---

Problem: src/habu/linker.f:4208 refuses a part's object when its entry root differs from the build's, and include.f ABS-OWNER (:1179-1185) picks an absolutely included file's root from its includer (the first of CURRENT$, ENGINE$, CWD$ above it, else its own directory). So the generated <out>/hb-build/<target>/end.f recompiles whenever one build's driver directory holds the out root and the next one's does not (cold through tools/hb-build.f, warm through a driver under the out root's parent), though end.f resolves nothing against its root and the rebuilt object equals the old one past INPUT. Any absolutely included file has the same exposure. Acceptance: a design in which a part's root is an input only when the part resolves a path against it, and a build that alternates those two drivers on one out root reuses every part; warm equals cold. Depends: none. Claim: unassigned.
