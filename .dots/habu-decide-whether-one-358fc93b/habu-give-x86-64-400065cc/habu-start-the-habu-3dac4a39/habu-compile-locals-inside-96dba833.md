---
title: Compile locals inside T0 quotations
status: closed
priority: 2
issue-type: task
created-at: "2026-10-28T13:35:40.409661+02:00"
closed-at: "2026-10-28T14:04:30.067817+02:00"
close-reason: "Not needed: the checker refuses a {: group inside a quotation (src/core/checker.f LOC-BEGIN, QDEPTH>0 -> LOC-REJECT), so no certified body has one; T0 keeps its -9580 refusal there. The bind-count check moves to its own dot."
---

Quotation bodies compile as deferred units after the definition (jerry-t0-quot); NFETCH:BIND-WIDTH numbers binds in source order (jerry-t0-locals). Merge must keep bind numbering in source order across deferred bodies, and check T0's bind count against NFETCH:BINDS as tier 1 BINDS-CK does. Workspace: jerry-t0 stack.
