---
title: Zero the DATA the other grown stores abandon
status: open
priority: 2
issue-type: task
created-at: "2026-10-25T06:59:21.864389+01:00"
---

Lane grow (ownyvokt) zeroes the DATA a grown registry store abandons (REG-AOT-ROOM path). USIGS-GROW, NORET-GROW and RECORD-LINK-ROOM also move stores and leave the abandoned DATA unzeroed; untraced whether a hit can grow them in a different order than the cold load. Reproduce with a file that grows each store past its baked capacity (GROW-HIT-CASE shape, test/native-file-types-e2e.f) and compare warm and fresh images.
