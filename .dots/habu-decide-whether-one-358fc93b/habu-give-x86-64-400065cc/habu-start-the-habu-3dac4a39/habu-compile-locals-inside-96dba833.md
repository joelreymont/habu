---
title: Compile locals inside T0 quotations
status: open
priority: 2
issue-type: task
created-at: "2026-10-28T13:35:40.409661+02:00"
---

Quotation bodies compile as deferred units after the definition (jerry-t0-quot); NFETCH:BIND-WIDTH numbers binds in source order (jerry-t0-locals). Merge must keep bind numbering in source order across deferred bodies, and check T0's bind count against NFETCH:BINDS as tier 1 BINDS-CK does. Workspace: jerry-t0 stack.
