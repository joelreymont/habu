---
title: Expect the moved Wasm image base
status: closed
priority: 2
issue-type: task
created-at: "2026-10-31T09:19:36.017797+02:00"
closed-at: "2026-10-31T12:11:58.084406+02:00"
close-reason: "Landed with the T0 release (ttloqwsx b17a1ee3): full suite 510/510, two generations converge, edit matrix F53/F105/F156 only, as master"
---

Workspace jerry-lead. wasm-link is the full suite's one red row (507/507 ran, 2026-10-31 09:14): BODY-SOURCE (vsqlmulv) adds 64008 bytes below DATA-START, so WPROF:IMAGE-BASE moves $311000 -> $321000 and test/wasm/link.f's six base-dependent expectations (MIN, ORDER, TYPES, data section, PAGES 50/51) fail. Fix: the module bytes at the new base, PAGES 51/52.
