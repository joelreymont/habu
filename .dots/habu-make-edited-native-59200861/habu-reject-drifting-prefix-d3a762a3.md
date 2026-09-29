---
title: Reject drifting prefix source closure
status: active
priority: 2
issue-type: task
created-at: "\"2026-09-29T13:51:55.408733+02:00\""
---

In tools/event-closure-lib.f, reject a missing loading dependency during cache discovery and reject newly appeared or changed loading-event membership when validating a retained closure. Preserve ordinary EC:BUILD semantics for existing consumers. Add focused behavior acceptance before code using the existing event-closure fixture, then run that fixture and the native-prefix source preflight without a native build. This is prerequisite to any cache key or checkpoint publication.
