---
title: Size attribution report for bin/hb
status: open
priority: 1
issue-type: task
created-at: "2026-07-18T14:34:37.676957+02:00"
---

docs/size-campaign.md rule 1. Build report emitting bytes per contributor: per-emitter-phase code (habu1 prims, habu2 kw JIT, MATCH family, P2WIDE), kwdata tables, LARITY/min-in guard rows, dict seed, page padding, signature; committed manifest rows gate-validated (gate-build-size.f style) incl. distance-to-page-floor per target. No compaction lands before this.
