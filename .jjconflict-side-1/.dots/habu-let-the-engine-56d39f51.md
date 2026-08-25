---
title: Let the engine nest quotations
status: open
priority: 2
issue-type: task
created-at: "2026-08-10T07:44:39.251782+02:00"
---

The engine exits 75 on [: [: ;] ;] because QPATCH/QENT/QXH are three single cells (layout.f:509-511), while the checker already certifies nested quotations (CF-QUOT/CF-SEMIQ have no depth limit - probed). Replace the three cells with a small stack in DATA; the exit-chain scoping (EXITH->QXH) nests with it. Acceptance: the nested probe compiles and runs under the engine; depth ceiling is a named refusal; existing single-level behavior byte-identical. Files: src/habu/{layout,habu2}.f + bootstrap mirror if the cells are boot-prefix. Depends: none (engine-side; independent of the chain lanes).
