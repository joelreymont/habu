---
title: Validate layout fetches in T0
status: closed
priority: 2
issue-type: task
created-at: "2026-10-29T02:45:23.247083+02:00"
closed-at: "2026-11-04T11:48:48.580551+02:00"
close-reason: "Census 2026-11-04, done: Landed e5c1c2f9 \"Validate layout fetches in T0\"; jerry-a3d not listed."
---

A3d (jerry-a3d, on ktwwkpqo): T0 never runs tier 1's VALIDATE-FETCH (elaborate.f: NFETCH:AT fact at an @ token -> width check, NFETCH-CHECK:TAGS call with the interned descriptor). Probe /tmp/jerry-a3c/c2/body.f: a sum-type slot of 99s, `BX-AT @ drop` dies under tier 1 at the fetch (rc 85, hb: bad layout tag) and returns rc 0 under T0. Found by A3c part 2's census. A3c (ktwwkpqo) itself: stack words and @/! move whole widths, E-T0-BUNDLE -9608; under review.
