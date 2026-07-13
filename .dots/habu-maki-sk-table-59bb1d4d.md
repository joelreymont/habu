---
title: "maki: SK table growth/eviction (E-SK-FULL at 32 keys)"
status: active
priority: 2
issue-type: task
created-at: "\"2026-07-13T11:00:25.364418+02:00\""
---

Replay-wiring residual (2026-07-13): SK-TAB-CAP=32; a long-lived schedules.rows with >32 distinct keys makes a session's first TILE throw E-SK-FULL (loud+named, fail-closed - but a capacity wall for real multi-model use). Options: REG-GROW1-style arena growth (precedent src/core/checker.f:56), LRU eviction (replay table is a cache - eviction is honest), or per-model scoping. Also the store file grows append-only with superseded rows (latest-wins on load) - compaction on write or load. After V2 sec 9 store lands this is superseded - check the epic first.
