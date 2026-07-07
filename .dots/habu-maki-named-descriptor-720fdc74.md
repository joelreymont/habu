---
title: "Maki: named descriptor planning vocabulary (CAD-PLAN 3)"
status: active
priority: 3
issue-type: task
created-at: "\"2026-07-06T15:15:38.902212+02:00\""
---

Clean forward step split from habu-compiler-pkg-scoped-1a4d29bd. CAD-PLAN section 3 wants model bodies written as the SAME source text (LINEAR GELU LINEAR) over a descriptor-typed PLANNING PACKAGE with lexical dispatch. Today only the parametric appenders exist (PLAN-UNARY/PLAN-BIN-EW/PLAN-MATMUL/PLAN-LINEAR/PLAN-TERN-EW take op-kind as an arg) plus PLINEAR/PGELU. Provide a named descriptor vocabulary in its OWN package (eager LINEAR/MATMUL/ADD already occupy MAKI, so a distinct planning package is required per section 3 "capture opens the planning package"): checked one-line wrappers GELU RELU SILU LAYERNORM RMSNORM SOFTMAX-ROW LINEAR MATMUL ADD MUL SCALE BIAS RESIDUAL-ADD CAST ROPE RESHAPE TRANSPOSE SLICE CONCAT GATHER, each ( tensor ... -- tensor ), over the existing appenders. This makes a hand-authored model block read as section-3 source and be checker-verified at top level (proven achievable in maki/plan-compose-test.f using the parametric words). NOT wired into MODEL: (that needs the reentrancy blocker habu-checker-reentrancy-certify-86771a6f). Broadcast-shape legality (cad.f SHP-CHECK) layers above the vocabulary as it does today. Owner: new maki/plan-vocab.f + test.

DELIVERED 2026-07-07 (fable workspace minion-descr):
READY NOW (no reentrancy needed):
- maki/plan-vocab.f: `package PLAN`, 20 checked one-line words, each ( tensor ... -- tensor ),
  re-exporting the existing MAKI appenders with the op-kind fixed (no op semantics duplicated):
  RELU/GELU/SILU/LAYERNORM/RMSNORM/SOFTMAX-ROW/CAST ( tensor -- tensor ) -> PLAN-UNARY;
  ADD/MUL/SCALE/BIAS/RESIDUAL-ADD ( tensor tensor -- tensor ) -> PLAN-BIN-EW;
  ROPE ( tensor tensor tensor -- tensor ) -> PLAN-TERN-EW;
  MATMUL ( tensor tensor -- tensor ) -> PLAN-MATMUL; LINEAR ( tensor tensor tensor -- tensor ) -> PLAN-LINEAR;
  RESHAPE/SLICE ( tensor n n -- tensor ), TRANSPOSE ( tensor -- tensor ),
  CONCAT/GATHER ( tensor tensor -- tensor ) -> the movement appenders.
- maki/plan-vocab-test.f (wired into maki/test.f): the plan-compose-test skip/fan-out DAG forms
  (PVT-SKIP, PVT-BRANCH) migrated onto the named vocabulary, checked at load, drivers assert the
  captured plan; CHECK-PASSES? negative fixtures prove the checker REJECTS malformed compositions
  at load (arity underflow on ADD/LINEAR/RESHAPE, non-tensor operand into GELU, leftover value),
  with positive controls. A hand-authored block opens `package PLAN` and bare LINEAR/GELU/... read
  as section-3 source, verified today. Gates green: maki/test.f 73 PASS/0 FAIL, test/run.f PASS,
  typed-local-diff-lint clean.
WAITS ON REENTRANCY (habu-checker-reentrancy-certify-86771a6f, via habu-compiler-pkg-scoped-1a4d29bd):
- lexically opening `package PLAN` as the scope of a MODEL: body so its bare ops compile as a
  checker-verified colon def during MODEL: execution. The vocabulary is ready to be that scope;
  only the reentrant compile-and-certify boundary is missing. Broadcast-shape legality (cad.f
  SHP-CHECK / E-CAD-PARAM-SHAPE) continues to layer above this vocabulary during capture, unchanged.
