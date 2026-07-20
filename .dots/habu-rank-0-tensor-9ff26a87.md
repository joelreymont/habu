---
title: "Rank-0 tensor accessors so scalar broadcast falls out of SPEC:"
status: open
priority: 2
issue-type: task
created-at: "2026-07-20T11:05:24.704049+02:00"
---

Recorded wall from the broadcast-forms landing (82941587, dot habu-spec-broadcast-forms-ad851424): a scalar 1x1 term in a SPEC: elementwise expression is a rank-0 factor tensor, and maki/extent-tensor.f's accessor generator cannot emit one - TENSOR: S ( ) dies E-UNDEFINED: x0 (verified) - and the scalar's full-sum adjoint is a rank-0 output with the same gap. Currently fails closed as E-SPEC-ARITY (correct interim). Fix at the root: teach extent-tensor.f to generate rank-0 accessors (a zero-index read/write pair over a 1-element span), after which the scalar form falls out of spec.f's existing suffix machinery with NO grammar change (the landing verified this analysis). Add: scalar broadcast golden (scale op parity vs SHP-SCALE-OK? class), its derived full-sum adjoint gradchecked, negatives for misuse, and remove the interim E-SPEC-ARITY wall test in favor of the working form. Territory: maki/extent-tensor.f, maki/spec.f dispatch if needed, spec tests.
