---
title: Package stdlib modules and gate bare loads
status: open
priority: 2
issue-type: task
created-at: "2026-07-19T17:44:34.788561+02:00"
---

Review finding 10 (pin 8195257e): 19 of 50 flat production lib modules fail standalone loading (hidden load-order coupling), 31 lack packages; lib/json-read.f:3 documents "Load after..." instead of requiring its deps, and its :139 global helper permits checked out-of-bounds reads. Fix: migrate flat modules to packages with explicit require rows, privatize unsafe helpers, and add a gate row that bare-loads every manifest module standalone (fail-closed on coupling regressions). Related: habu-seal-owners-syntax-63051652 / habu-seal-owners-migrate-2dda16df (sealed owners ride on packaged modules).
