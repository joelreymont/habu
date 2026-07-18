---
title: "Decide MODEL: retirement once SPEC: carries attention"
status: open
priority: 2
issue-type: task
created-at: "2026-07-18T23:21:32.750976+02:00"
---

Two tensor stacks coexist: the 2D CAD stack (maki/tensor-value.f, maki/model-ir.f, the MODEL: single-running-value DSL) and the extent-typed stack (EXTENT:/TENSOR:/SPEC:). The BTC design keeps the 2D IR deliberately (memory layout is 2D); what has no end date is the MODEL: authoring surface itself. Decision proposed for ratification: MODEL: retires as an authoring surface once nanoGPT's attention forward pass runs from SPEC: lines matched against the maki/attention.f golden (dot habu-fix-model-dsl-d066701e acceptance); after that, remaining MODEL: users migrate opportunistically and the DSL words (PARSE-SHAPE, CAP-TOKEN running-value threading) delete rather than gaining batch features. Until then MODEL: gains NO new capabilities - the fix-model-dsl dot already points multi-operand work at SPEC:. Record the ratified decision here, then mint the migration/deletion dots when the acceptance test exists.
