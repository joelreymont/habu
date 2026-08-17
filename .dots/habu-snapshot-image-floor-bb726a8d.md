---
title: Snapshot image floor should be persist-time ndict
status: open
priority: 2
issue-type: task
created-at: "2026-08-17T22:34:33.300562+02:00"
---

Found by single-prefix-2 (2026-08-17): the snapshot image's seal floor is the floor the build had, leaving a forgettable-engine-record window (239 records post-fix, 306 on master - the fix TIGHTENED it but the hole is pre-existing). The floor an image persists should be its own ndict at persist time, closing the window to zero. Small, structural; regression = an image whose floor equals its record count.
