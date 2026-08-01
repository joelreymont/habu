---
title: Fuse comparisons into branches
status: active
priority: 2
issue-type: task
created-at: "\"2026-08-01T15:39:39.942714+02:00\""
---

Phase 2, first quality pass, the largest measured waste in the generated code: a source comparison feeding a conditional branch compiles today as a64.flag (cmp;cset;neg - materialize the Habu flag) then a64.cbz (cbz;b) - five instructions and one register where cmp;b.cond;b is three and none. Wanted: when a comparison's ONLY use is the branch test that follows it, select the fused form - a new a64.cmpbr operation (compare two registers under a condition, two successors; instruction count 2: cmp + b.cond, plus the unconditional successor branch as today) - leaving a64.flag for comparisons whose result is USED as a value (MAX2's 2dup < feeding if consumes it as the branch test - check each corpus body for which shape it has). The single-use fact must be derived structurally (the value's use count off the frozen module, the same liveness machinery the allocator has), never guessed. Where the fusion lives: argue selection-time pattern (the selector sees hir.lt feeding hir.brz in the source module) versus a separate module-to-module pass in spill.f's style; one authority, validator re-derives whichever. Acceptance: the full 11-row table re-run - loop rows (SUM-TO, COUNT-DOWN, BYTE-SUM, BYTE-FIND, FACT) must show measurably fewer bytes AND lower cost with identical results; any row that does not move is reported honestly; if the pass moves nothing overall it is REVERTED per the plan's rule. Mutations: fused branch polarity, fusion applied to a multi-use comparison (must be refused or fall back to a64.flag - proven by a fixture where the flag value is also returned), condition mapping per operator - die through execution or named refusal.

Claim: agent=fuselane workspace=.jj-ws/habu-fuse-comparisons-into-a10e7fb3
