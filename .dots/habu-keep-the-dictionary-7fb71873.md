---
title: Keep the dictionary hash index alive
status: open
priority: 2
issue-type: task
created-at: "2026-08-05T10:36:19.583058+02:00"
---

CG-25. src/habu/habu1.f:2777-2805 treats a reused stale ID as live after NDICT regrows, consumes another slot, and eventually zeroes HIDXP — the process silently drops to linear FIND for its remaining life (measured: checked batch 3.691 ms -> 13.651 ms after 60,000 rollback cycles). test/engine-suite.f:1963-1986 ends with a constant equality and never observes the index. Fix: maintain exact occupancy/load and compact/rebuild from live [0, NDICT) before legal rollback history can cross the structural load bound, or implement correct rollback deletion; never silently disable the index. Test must inspect HIDX after crossing the former capacity.
