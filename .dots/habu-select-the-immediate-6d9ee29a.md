---
title: Select the immediate form the encoder ships
status: active
priority: 2
issue-type: task
created-at: "2026-08-06T19:16:20.448344+02:00"
---

24 of the corpus loops' 42 in-loop constants are add/sub immediates the chain materialises into a register and then uses once — ENC-ADDI already ships, so SELECTING the immediate form saves the instruction and costs no register, where hoisting would save it and cost one (hoist lane inventory, 2026-08-06, merged 8f67723f — the per-row counts are in the inventory suite). A selection-stage transform: when an operand is a constant fitting the immediate field of the consuming op, emit the immediate form and never materialise. Measure-first per standing rule: name the rows from the inventory, predict deltas, all answers bit-for-bit, no untouched row moves, both gaps, deliberate re-pin. Claim: agent=immsel2 workspace=.jj-ws/habu-select-the-immediate-6d9ee29a
