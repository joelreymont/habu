---
title: Canonicalise DATA-region snapshot pointers
status: open
priority: 2
issue-type: task
created-at: "2026-07-29T19:58:29.556568+02:00"
---

Full context: PRIORITY 1, blocks habu-fix-owner-wid-e2bc360c. EM-SNAPSHOT-COPY-DATA (src/habu/habu2.f:4057-4063) restores the DATA payload verbatim, and only RBASE-CELL, S0-CELL, ARGC/ARGV/ENVP, NDICT and CP are re-stored afterwards. Any other DATA cell holding a live region pointer keeps the WRITER's address — harmless while the region lived at a fixed VA, broken under SNAP v4. Evidence: with the region-to-text displacement matched between writer and reader, a plain snapshot image STILL crashes, indicating at least one further un-canonicalised class. Audit every DATA cell that can hold a region address, canonicalise it to the RBASE-VA sentinel on write and rebase it on restore, and add a checked regression that boots a snapshot image and asserts each such cell points inside the live region bounds.
