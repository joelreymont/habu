---
title: "maki: typed no-slot sentinel for MP-SLOT"
status: active
priority: 2
issue-type: task
created-at: "\"2026-07-13T00:09:56.910329+02:00\""
---

TK-CELL migration residual (2026-07-13): maki/mem-plan.f:83 MP-SLOT mixes a raw -1 sentinel with MIR:input-slot values ('-1 MP-SLOT !'), so it cannot become a typed 1-slot LAYOUT-BUFFER until 'no slot' has a typed representation. Options: option-shaped cell (needs option<MIR:input-slot> storage - check TDECL payload rules), a dedicated variant family (mp-slot = none | some slot), or a trusted sentinel mint documented as a boundary. Small; unblocks the last untyped scratch cell in the mem-plan path. After the TK-CELL capability lands.
