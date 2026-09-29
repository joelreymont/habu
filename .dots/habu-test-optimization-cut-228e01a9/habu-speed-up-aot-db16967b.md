---
title: Speed up aot-data-cell-refusals and the tic6x asm row
status: open
priority: 1
issue-type: task
created-at: "2026-09-29T18:01:45.776504+02:00"
---

Problem: aot-data-cell-refusals CASE-JSON (test/compiler/aot-data-cell-refusals.f:145-157) rebuilds CASE-PRE-XT's identical subject; compiler-tic6x-asm re-emits its helper on every simulated call (tic6x-eabi.f:55-58, 121-135, 252-255; ~7,700 emissions, 55% of the file). Evidence: ~/.cache/tmp/kestrel-gate/test-review/L2-compiler-other.md. Acceptance: CASE-JSON reuses the CASE-PRE-XT build; the tic6x helper is emitted once per group; every assertion kept. Files/Ownership: test/compiler/aot-data-cell-refusals.f, the tic6x test files named in the report. Base: 614ae0ba (row-split stack head, not yet on master). Verify: every touched row passes standalone (bin/hb --load <row file>); a mutation of one moved or rewritten assertion fails; report per-row seconds before and after. Depends: none. Claim: unassigned.
