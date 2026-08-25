---
title: "Checker: shape/kind-indexed tensor type for CAD composition"
status: open
priority: 3
issue-type: task
created-at: "2026-07-06T15:15:19.191238+02:00"
---

Follow-on for habu-compiler-pkg-scoped-1a4d29bd. maki tensor is a single opaque DEFTYPE (maki/tensor-value.f), so a checked composition of descriptor ops can only prove stack ARITY and tensor-vs-nontensor - NOT op KIND (every op shares the tensor type) and NOT SHAPE legality (RxC / dtype / layout are runtime record fields, not in the type). Consequence: E-CAD-PARAM-SHAPE (broadcast legality) and op-kind/shape mismatches can NEVER become checker diagnostics as long as tensor is shape-opaque; they stay runtime throws. To make the section-3 goal "arity/KIND errors are checker diagnostics" fully real, the checker needs shape/dtype/layout-indexed tensor types (dependent/parametric type params, e.g. tensor<R,C,dt,lay>) or a refinement mechanism. Capability: parametric type params carrying static extents + unification over them. Scoped separately because it is a checker type-system extension independent of the reentrancy blocker. Proof today: maki/plan-compose-test.f shows the checker catching arity/type of a descriptor composition but NOT shape (shape is asserted at runtime).
