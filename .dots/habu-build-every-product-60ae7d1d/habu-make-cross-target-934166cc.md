---
title: Make cross-target data symbolic
status: open
priority: 2
issue-type: task
created-at: "2026-10-03T22:32:28.832784+03:00"
---

Problem: frozen HIR carries host addresses with kind tags (hir.f:548-565, 822-848) and the only cross path maps them back to records at capture (aot-shadow.f header); create/here/,/allot have no realizable cross-compile path except that audited adapter (PA-r2 §7, P3). Acceptance: TargetRef algebra and TargetObject builder; owner-instrumented create/here/,/allot; the capture path kept as the admitted legacy adapter; T10-T14 and N07-N11 pass; no host pointer in a foreign artifact. Files: src/compiler/native/{elaborate,hir,shadow}.f, src/compiler/artifact/ (new), src/habu/aot-shadow.f, tools/native-build-core.f. Verify: full gate; the T13 mutation (a host pointer hidden in initializer bytes) refused. Depends: habu-give-each-backend-b6f7ea4f. Ownership: src/compiler/artifact/, elaborate.f (coordinate with Heron). Lane: dave. Claim: unassigned.
