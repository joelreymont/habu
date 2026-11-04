---
title: Build the cold product when a cast file is reused
status: closed
priority: 2
issue-type: task
created-at: "2026-10-25T03:20:47.290638+01:00"
closed-at: "2026-10-28T17:13:33.667573+02:00"
close-reason: landed 8e8b9ce1, 7ee51123; state e2e compares cold-hb with warm-hb in the gated row
---

State e2e BARE-CASE SAME-FILE (case 112) fails at the carl-int tip ruxwrsmt: cold-hb and warm-hb differ in 2436530 bytes (one at 10094456, a one-byte shift from ~10532860). Bare lane checked only the code blob. Worker lane in .jj-ws.noindex/carl-bsame, brief ~/.cache/tmp/carl-file/brief-bsame.md.
