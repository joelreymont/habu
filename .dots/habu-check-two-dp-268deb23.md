---
title: "Share the DP rewind, drop the REPL's second"
status: active
priority: 2
issue-type: task
created-at: "2026-11-17T11:02:55.282529+01:00"
---

Problem: two DP rollbacks are duplicated (both harmless today, by probe): src/core/generated-declaration-dictionary.f:17 DICTIONARY-DP! copies interpreter.f FRAME-ROLLBACK-CODE's DP tail without ADDRESS-CELLS:KEEP-BELOW, sound only while the declaration generator makes no address-cell row, which nothing checks; and REPL-LINE's FRAME-UNWOUND case reruns FRAME-ROLLBACK-CODE after the engine's eval-throw recovery rolled the same frame back. Acceptance: one DATA-rewind word both rollbacks call; the REPL case does what EVAL-FRAMED-RANGE's does; the engine converges and the rollback tests pass. Depends: none. Claim: agent=carl workspace=.jj-ws.noindex/carl-gd2 and .jj-ws.noindex/carl-gd3.
