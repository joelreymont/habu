---
title: Resolve native REPL DATA comparison failure
status: open
priority: 1
issue-type: task
created-at: "2026-10-14T17:45:00.207397+03:00"
---

Unchangedc316 source baseline tools/hb-build-aot-test.f fails existing BUILD-AOT-SPAN-ENGINE/F112: HBT-DIFF-AT returns1094 instead of-1. Same failure observed with maker-hit reporting patch; report-only change does not cause it. Determine actual differing cells/semantic owner and whether the compared DATA equality is a real product requirement or a redundant implementation mirror. Inspect existing downstream E2Es before new tests. Fix responsible behavior if genuine; otherwise remove/consolidate the useless assertion with concrete coverage evidence, asking Joel only for ambiguous product requirements. Keep unrelated maker reporting fix separate. Acceptance: exact before/after real build artifacts and meaningful behavior/refusal checks, proportionate required native/convergence if baked capture/codegen change, Astra review for substantive fix.
