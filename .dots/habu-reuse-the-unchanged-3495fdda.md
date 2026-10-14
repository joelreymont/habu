---
title: Reuse the unchanged saved maker
status: open
priority: 1
issue-type: task
created-at: "2026-10-14T16:55:46.477307+03:00"
---

Otto reports maker_hit false on every edit and the6.4MB hb-aot-maker image rewritten under the same key. Reproduce /tmp/otto-incremental-repro.sh and isolate actual KEYED-IMAGE selection/publication failure; no caller workaround or deferred unified-driver dependency. Before implementation inspect existing AOT image/cache E2Es and add a distinct red edit-build reuse case if missing. Acceptance: unchanged maker selected on real edit builds, no rewrite under same key, resulting edited executable runs; real source/support changes invalidate it; focused existing cache E2Es and independent Astra review.
