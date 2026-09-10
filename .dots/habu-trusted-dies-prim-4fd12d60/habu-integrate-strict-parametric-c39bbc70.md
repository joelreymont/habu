---
title: Integrate strict parametric effects and checked Forth helpers
status: open
priority: 1
issue-type: task
created-at: "2026-09-10T18:03:13.337312+03:00"
---

Owner: Cedar; nominal_pointer handles the remaining reduced compiler binding failure. Strict nominal/generic pointers and type-variable kinds are integrated. Concrete byte/cell/record declarations replace false generic outputs; accepted nominal-input instantiation, rejected output refinement, typed stores and public tool paths have focused passing tests. Parsed CHECK! publication now preserves live type terms for lowering certificates.

Finish the remaining honest declaration migration exposed by warm replay and production library loads. Do not weaken parametric checking. Keep rejected-program coverage and run the final combined checker/native suites. The old load-rejection harness still needs diagnosis of E-JSON-SYNTAX after strict fixes; generated-declaration capacity preflight also has a remaining baseline failure.
