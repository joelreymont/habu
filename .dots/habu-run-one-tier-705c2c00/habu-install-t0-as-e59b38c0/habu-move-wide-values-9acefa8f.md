---
title: Move wide values through T0 stack words
status: open
priority: 2
issue-type: task
created-at: "2026-10-29T02:21:18.127752+02:00"
---

A3c (jerry-a3c, on qtrktsss): T0 lowers drop/dup/nip etc. one cell per operand, so a multi-cell value (PRODUCT, wide payload) is silently miscompiled: probe /tmp/jerry-a3b/q/probe-drop-t0.f ((n pq -- n) drop, (pq n -- n) nip, (pq -- pq pq) dup wrong under T0, right under tier 1); tier 1's native-match.f assert 55 under T0. Fix: read the checker's per-token operand widths (WF-XPORT-RECORD via NDICT:OPERAND-CELLS) and move whole widths; refuse where no width is known. Found by A3b1 running tier 1's MATCH tests under T0. Also noted there: T0 code exceeds tier 1's size bounds in native-match.f asserts 138/139 (E-UNW 140 > 128, E-QUAD 228 > 184): A5 measurement input.
