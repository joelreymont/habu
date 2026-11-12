---
title: Ask Joel about public words without callers
status: open
priority: 2
issue-type: task
created-at: "2026-10-31T09:13:41.799673+02:00"
---

One interview question (context inside it) on each public word with no production caller: keep, make private, or delete with its tests. Words still defined on master: GENIO FAULT@, LATCH, LIVE-DEVICE, FORWARD, FORWARD-EMIT (src/lib/genio-core.f); LIB-ROW@, FN-ROW@, SYMBOL-CAP and PENDING (src/lib/ffi-abi.f); CHECKER-OWNER:UNIT-FACTS (src/compiler/native/checker-owner.f:404); REG-POINTERS-CLEAR (src/core/checker.f:15706, a global outside any package, with no caller left). Zen (phil, 2026-10-31: Zen is no longer an MCP client, runs no turn tasks and exchanges no tokens) will not call HTTP:REPLY, HTTP:SERVING?, the CREW words, MCP:CANCEL, MCP:TOKEN!, BEARER:CLIENT! or BEARER:TOKEN; ask tenderd's and Maki's owners whether they call the token words or MCP:VISIBLE! before listing those. Recheck each against the landed tree first. Claim: agent=carl, no workspace.
