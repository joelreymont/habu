---
title: Check two DP rollback paths
status: active
priority: 2
issue-type: task
created-at: "2026-11-17T11:02:55.282529+01:00"
---

Problem: src/core/generated-declaration-dictionary.f:17 DICTIONARY-DP! lowers DP without ADDRESS-CELLS:KEEP-BELOW, which interpreter.f FRAME-ROLLBACK-CODE calls; and arm64 REPL-LINE's FRAME-UNWOUND case runs FRAME-ROLLBACK-CODE after the engine already rolled back. Acceptance: each is shown harmless by a probe, or its harm is reproduced and fixed. Depends: none. Claim: agent=carl workspace=.jj-ws.noindex/carl-gd1 (hunt).
