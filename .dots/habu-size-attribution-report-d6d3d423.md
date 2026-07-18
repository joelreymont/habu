---
title: Size attribution report for bin/hb
status: open
priority: 1
issue-type: task
created-at: "2026-07-18T14:34:37.676957+02:00"
---

docs/size-campaign.md rule 1. Build report emitting bytes per contributor: per-emitter-phase code (habu1 prims, habu2 kw JIT, MATCH family, P2WIDE), kwdata tables, LARITY/min-in guard rows, dict seed, page padding, signature; committed manifest rows gate-validated (gate-build-size.f style) incl. distance-to-page-floor per target. No compaction lands before this.

Progress 2026-07-18: the first slice landed from the spark lane — tools/size-report.f (+ test, + main driver), which parses the per-phase byte map that src/habu/habu2.f emits under HABU_ENGINE_SIZE_MAP and renders the rows, code total, engine file size, header/pad remainder, and distance to the page floor. Reviewed and merged on the Mac. REMAINING to close this dot: exact byte reconciliation (every byte of bin/hb attributed; rows must sum to the file size exactly, with any gap itemized rather than lumped into a remainder line) and the committed-manifest gate wiring (gate-build-size.f style rows validated in the gate, per target). The spark session's gap analysis has the detailed completion spec; ask it (or re-derive from the tool + emitter) before dispatching the closing slice.
