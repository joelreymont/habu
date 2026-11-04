---
title: Zero the DATA the other grown stores abandon
status: closed
priority: 2
issue-type: task
created-at: "2026-10-25T06:59:21.864389+01:00"
closed-at: "2026-10-31T20:52:54.848287+02:00"
close-reason: "Landed in lane 11 4b54ed21: a grown checker store leaves no dead bytes (ARENA-UNMAP/ARENA-ABANDON)."
---

Lane grow (ownyvokt) zeroes the DATA a grown registry store abandons (REG-AOT-ROOM path). USIGS-GROW, NORET-GROW and RECORD-LINK-ROOM also move stores and leave the abandoned DATA unzeroed; untraced whether a hit can grow them in a different order than the cold load. Reproduce with a file that grows each store past its baked capacity (GROW-HIT-CASE shape, test/native-file-types-e2e.f) and compare warm and fresh images.

Cause (probe U7b, carl-probe/.out/probe/b2da-report.md): USIGS-GROW leaves its old DATA span unzeroed; from N = 1331 signed colon definitions the half-built record that triggers the grow is written differently cold (symbol 0) and warm (real symbol), so one dead heap cell differs. NORET-GROW and RECORD-LINK-ROOM share the gap.

Claim (carl, 2026-10-30): fix worker in .jj-ws.noindex/carl-fgrow on spvqktuy, brief carl-dots/.out/carl-file/brief-fix-grow.md.
