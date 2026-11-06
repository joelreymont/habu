---
title: Retire an MCP exchange through one CLEANUP
status: open
priority: 2
issue-type: task
created-at: "2026-11-07T04:26:38.625214+02:00"
---

Problem: src/lib/net/mcp.f RETIRE (about :1262-1275) cancels a transfer still in flight, catches CANCEL's E-STATE, drains the handle and then CLEANs it up: a catch that decides what happens next. Since 'Give up a model call's uncollected transfer' (phil, 2026-11-07), CURL:CLEANUP gives a record up first (ABANDON), so a transfer nobody will collect is given up by CLEANUP alone (phil's follow-up). Acceptance: RETIRE calls CURL:CLEANUP on a handle with or without a transfer in flight and keeps no catch or drain, if CLEANUP's contract covers a transfer in flight; mcp's tests unchanged and passing; a case retiring a row mid-transfer. Files: src/lib/net/mcp.f, its test. Verify: bin/hb --load test/run.f -- mcp curl-http. Depends: none. Ownership: phil (model and MCP lane) or carl. Claim: unassigned.
