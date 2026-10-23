---
title: Delete disabled recovery extension machinery
status: closed
priority: 2
issue-type: task
created-at: "2026-10-16T11:17:41.836203+03:00"
closed-at: "2026-10-22T12:36:34.961470+03:00"
close-reason: "Triage 2026-10-22, DONE (STALE-CLAIM): BUILD-EXT is gone, and BF-PREFLIGHT (build-fixpoint.f:589) only runs BF-REQUIRE-WATERMARK."
---

build-fixpoint BUILD-EXT controls are initialized empty, never set then undefined; raw source preflight searches exact error text/create spellings without proving behavior. Delete unused extension package and spelling check/helpers; retain actual allocator and host/layout boundaries. Existing build driver fixtures and recovery check as required; coordinate Carl driver ownership.
