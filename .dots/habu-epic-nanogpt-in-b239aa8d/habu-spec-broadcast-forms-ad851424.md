---
title: "SPEC: broadcast and elementwise forms"
status: open
priority: 1
issue-type: task
created-at: "2026-07-19T08:21:55+02:00"
---

The shipped SPEC: grammar is contraction-only (maki/spec.f:425 `OUT[free] = factors [*] +SUM ct`), so the three broadcast shape classes the Model CAD checker already legalizes (docs/nanogpt-inventory.md Broadcasts section; cad.f:338 SHP-LEGAL?) cannot be authored as SPEC: lines yet: row broadcast 1xC over rows (bias add, SHP-ROW-OK?), scalar 1x1 broadcast (scale, SHP-SCALE-OK?), and same-shape elementwise (residual/add/mul). These are needed by the multi-head attention sublayer (output projection bias, residual adds - habu-multi-head-self-a1e0692f) and the GPT-2 block (habu-gpt-2-block-a9039501). Design the grammar extension inside the existing recursive-descent parser (maki/spec.f SP-PARSE-*): an output-shaped expression with + / elementwise * where a factor's index list is a suffix of the output's (row/scalar broadcast falls out of rank difference), keeping +SUM contraction composable with it. Derive the same three artifacts SPEC: already derives (checked candidate golden, planner dataflow record, PROMOTE shape obligations) and mirror maki/spec-attention-test.f's proof structure: golden numeric parity against an existing maki op, dataflow-record assertions, named-throw negatives (reuse E-SPEC-SYNTAX/ARITY/EXTENT/TENSOR), and a checker reject of a shape-illegal broadcast. Follows the closed habu-fix-model-dsl-d066701e (its closure note records the coverage audit).
