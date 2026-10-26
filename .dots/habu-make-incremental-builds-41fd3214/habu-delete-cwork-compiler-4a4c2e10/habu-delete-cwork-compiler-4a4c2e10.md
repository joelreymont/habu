---
title: Delete CWORK; compiler work lives in the context region
status: active
priority: 1
issue-type: task
created-at: "2026-10-26T13:52:27.651918+02:00"
---

Joel 2026-10-26: compiler working data is never cached, captured or carried. It is bump-allocated in the compile context's region and discarded when the context's release moves the cursor back. Delete src/compiler/workspace.f and every CWORK word. Per-stage records live in the context region, session records in the session context, driver latches become plain variables nulled after each request, registries become DATA tables of offsets. test/native-file-cwork-e2e.f (F9 tests dead file-cache machinery) goes with it. Design: ~/.cache/tmp/carl-file/cwork-design.md, lanes 0-8: 2-6 parallel after 0-1, 7 after 5-6, 8 last. Acceptance: no CWORK word remains; full gate; multi-generation convergence. Lead: carl.
