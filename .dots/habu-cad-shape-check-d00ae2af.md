---
title: "CAD: shape-check binary elementwise param operands"
status: active
priority: 2
issue-type: task
created-at: "\"2026-07-04T10:48:14.569112+02:00\""
---

Worker finding (named-refs lane): PLAN-BIN-EW / RESIDUAL-ADD capture does not validate that the parameter operand's shape matches the data operand (bias 1xC broadcast is intentional; residual-add requires same-shape). A mismatched residual currently plans without a capture-time error and only fails later (if at all). Fix: capture-time shape legality per op class in maki/plan-ops.f or CAP-OP (residual/add/mul same-shape or documented broadcast classes; named throw in the cad range), negative tests in cad-ref-test.f. Small.
