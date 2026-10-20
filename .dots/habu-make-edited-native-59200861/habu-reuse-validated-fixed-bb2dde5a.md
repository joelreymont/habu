---
title: Reuse validated fixed-builder identity
status: closed
priority: 2
issue-type: task
created-at: "2026-10-20T22:34:23.360924+03:00"
closed-at: "2026-10-20T22:45:55.136875+03:00"
close-reason: Fixed AOT-builder selection consumes the already validated producer identity and folds its actual recipe and selected root without another closure walk. Standalone selection keeps discovery. Existing image, cache and producer freshness/race E2Es pass; independent Astra review clear. Matched selector CPU fell 1.2676 seconds to 0.000114; whole root and leaf build CPU fell 4.05 to 2.81 and 3.97 to 2.68 seconds with identical binaries, maps and runtime output.
---

Whole edited Dots build measures 1.329 seconds parent thread CPU selecting its fixed saved AOT builder, mostly rediscovering producer closure. HBB-MAKER-KEY! has already validated the complete producer input manifest and computed a producer identity in the same command. Select the fixed builder from that existing validated identity instead of rediscovering the overlapping source closure. Preserve actual source/path-query freshness, selected engine/map/target/root identity, standalone builder selection, invalidation and runtime behavior. First inspect existing AOT image/cache E2Es, show pre-code redundant discovery through an existing real build, then implement at the owning selector boundary; require exact products, focused independent review and actual warm CPU before/after.
