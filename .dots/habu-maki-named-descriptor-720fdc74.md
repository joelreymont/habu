---
title: "Maki: named descriptor planning vocabulary (CAD-PLAN 3)"
status: open
priority: 3
issue-type: task
created-at: "2026-07-06T15:15:38.902212+02:00"
---

Clean forward step split from habu-compiler-pkg-scoped-1a4d29bd. CAD-PLAN section 3 wants model bodies written as the SAME source text (LINEAR GELU LINEAR) over a descriptor-typed PLANNING PACKAGE with lexical dispatch. Today only the parametric appenders exist (PLAN-UNARY/PLAN-BIN-EW/PLAN-MATMUL/PLAN-LINEAR/PLAN-TERN-EW take op-kind as an arg) plus PLINEAR/PGELU. Provide a named descriptor vocabulary in its OWN package (eager LINEAR/MATMUL/ADD already occupy MAKI, so a distinct planning package is required per section 3 "capture opens the planning package"): checked one-line wrappers GELU RELU SILU LAYERNORM RMSNORM SOFTMAX-ROW LINEAR MATMUL ADD MUL SCALE BIAS RESIDUAL-ADD CAST ROPE RESHAPE TRANSPOSE SLICE CONCAT GATHER, each ( tensor ... -- tensor ), over the existing appenders. This makes a hand-authored model block read as section-3 source and be checker-verified at top level (proven achievable in maki/plan-compose-test.f using the parametric words). NOT wired into MODEL: (that needs the reentrancy blocker habu-checker-reentrancy-certify-86771a6f). Broadcast-shape legality (cad.f SHP-CHECK) layers above the vocabulary as it does today. Owner: new maki/plan-vocab.f + test.
