---
title: Skip impossible mapped-pointer searches
status: closed
priority: 2
issue-type: task
created-at: "2026-10-20T21:57:04.569825+03:00"
closed-at: "2026-10-20T22:36:51.826733+03:00"
close-reason: Reject values outside the existing sorted kernel-map bounds before binary search, preserving holes, half-open endpoints, scratch reset and bytewide raw scanning. Real kernel boundary, cell-body and stripped pointer E2Es pass; independent Astra review clear. Matched Dots root and leaf user CPU fell 18.6 and 16.6 percent.
---

Real Dots stripped edit profiles attribute51-55percent of final-link samples to PROC-MAPS:ROW-AT. Of5928321 overlapping DATA windows,5928175 values lie outside the first/last kernel-listed mapping bounds. Add the sorted-map outer-bound rejection before binary search, deriving bounds from the existing live table and preserving empty-map behavior, half-open intervals, bytewide raw scanning, actual mapped-pointer refusals and capture/reload lifetime. Measure identical root/leaf builds before/after with exact image/map equality, existing proc-maps and raw-pointer/CELL-BODY E2Es, and independent review; no address heuristics or cached stale DATA bounds.
