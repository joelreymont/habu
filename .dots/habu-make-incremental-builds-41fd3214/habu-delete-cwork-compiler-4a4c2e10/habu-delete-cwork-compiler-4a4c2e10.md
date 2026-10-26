---
title: Delete CWORK; compiler work lives in typed arena records
status: active
priority: 1
issue-type: task
created-at: "2026-10-26T13:52:27.651918+02:00"
---

Joel 2026-10-26: compiler working data is never cached, captured or carried. Approved redesign (Joel, interview 2026-10-26; carl + tim): each pass owns its working state as a typed record taken from the compilation's MEM:arena scope and passed on the stack; no module global or DATA cell points into a scope; scope reset discards it; the IR region moves onto MEM:arena. Rev 1 (IR-CTX:RECORD) and rev 2 (WORK-* definers) are dropped. Design ~/.cache/tmp/carl-file/wstate-recommend.md. Order: tim's MEM:arena, then IR onto it (measured), then the typed definer, then lanes 2-4 in parallel, then lane 5 (delete). Acceptance: no CWORK word or record key remains; full gate; multi-generation convergence. Lead: carl.
