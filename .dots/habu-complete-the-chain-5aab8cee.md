---
title: Complete the chain dialect to the engine surface
status: open
priority: 2
issue-type: task
created-at: "2026-08-06T16:07:09.755260+02:00"
---

The chain models 61 spellings vs the engine's 70 compile-path keyword rows over 174 primitives (thecut audit, hir-word.f:938). Missing, by measured refusal: string/char literals (E-HIR-KIND), case/of/endof/endcase, ADT match/construct (needs the aggregate substrate), quotations, does>, plain do/+loop/leave/j, >r/r>/r@, execute, and ordinary primitives (negate 0< mod abs min max +! ...). Tranche the work by refusal count over the real stdlib (measure which gaps block the most definitions — compile the tree through the chain in no-emit mode once it exists and count refusals by shape), land tranches biggest-first, each with corpus rows per the measure-first rule. Blocks the cut.
