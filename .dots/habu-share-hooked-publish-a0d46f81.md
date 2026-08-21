---
title: Share hooked publish emitter
status: open
priority: 2
issue-type: task
created-at: "2026-07-17T13:50:58.884108+02:00"
---

Stop-line child of habu-shrink-the-c-721f214d. src/habu/habu2.f:5559 emits EM-COMPILE-PUBLISH-HOOKED twice for mutually exclusive dispatch legs, duplicating target code inside compile/semi. Emit one shared hooked tail behind a label while preserving HOOK-CELL and TSIG-U-CELL routing. Measure exact region/file delta with HABU_ENGINE_SIZE_MAP, add or identify path regressions for hooked, trusted, rejected, and pass-2 publishing, run focused native engine/fixpoint gates, typed-local diff lint, then close only after landed green.

REOPENED 2026-08-04 (dot-purge): this dot carried `status: active` with no live owner - no `agent=`/workspace claim, or a claim explicitly released. An active dot with no owner is invisible to `dot ready` and holds its id hostage, so the status is now `open` and the dot is free to claim.
