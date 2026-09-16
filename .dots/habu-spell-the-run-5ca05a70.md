---
title: Spell the run-row width once and name the unowned tail
status: open
priority: 2
issue-type: task
created-at: "2026-09-16T16:51:15.419689+03:00"
---

Problem: four sites still spell the captured run-row width as a literal 8 rather than AOT-WINDOW:RUN-ROW (src/habu/aot-decl.f:477, aot-file.f:284 and :672, habu2.f:9568), semantically identical today and a drift hazard after ec569af9 made RUN-ROW the format authority; and tools/data-table-census.f attributes the DP heap above the last create-d word to TR-LASTZERO while tools/engine-size.f names it USIGS-USER, so the two instruments disagree on the largest owner. Acceptance: the four sites read RUN-ROW; the census names an unowned tail by the pointer cell that targets it (the address-cell table, as engine-size does) and both tools print the same owner for that region. Files: src/habu/aot-decl.f, aot-file.f, habu2.f, tools/data-table-census.f. Verify: both tools on the release engine; tools/native-build.f fixpoint. Depends: none. Ownership: capture format. Claim: unassigned.
