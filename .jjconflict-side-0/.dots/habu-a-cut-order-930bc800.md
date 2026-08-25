---
title: A cut-order census
status: open
priority: 2
issue-type: task
created-at: "2026-08-13T02:03:05.422840+02:00"
---

Instrument gap (pool pricing, merged a89f786b): NCLOB:ROWS is 0 before and after a census run, so the census prices every record-conditioned mechanism against the WORST case by construction - callees are never published before their callers are measured. A cut-order census (publish each file's callees through the chain before measuring callers, dependency order) would report the real post-cut gap. Files: tools/chain-census*.f. Depends: none.
