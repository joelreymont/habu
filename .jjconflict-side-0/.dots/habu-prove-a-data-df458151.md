---
title: Prove a data-stack entry and exit against the module it was lowered from
status: open
priority: 2
issue-type: task
created-at: "2026-07-31T22:46:28.189374+02:00"
---

src/compiler/native/regalloc-verify.f's DSTACK-CK decides everything about a data-stack entry and exit that one module and one contract can decide: the pointer moves down over exactly the declared arguments and up over exactly the declared results, each load names the slot its argument place names, each store names the slot its result place names, and nothing else in the block touches the data stack. What it cannot decide is that the value a store publishes is the value the program computed for that result - that is a statement about the module the selector READ, and the validator is handed one module. This is the same gap the spill lowering has (dot habu-prove-the-spill-0294e0e8) with the same shape of answer: a pass that reads both modules and compares them, or a lowering certificate the validator can check. Owners: A64SEL, A64RAV.
