---
title: Drop dead owner rows; FMT and SOURCE tests own packages
status: active
priority: 2
issue-type: task
created-at: "2026-11-12T17:44:39.038773+02:00"
---

Problem: src/core/checker-surface.f is required by nothing yet its owner row (src/habu/prims.f:962) makes package CHECKER-SURFACE; AOT-CAPTURE exists in hb only through prims.f:965, so aot-capture.f:30 and aot-shadow.f:15 reopen it; src/lib/fmt-test.f:13 and src/lib/source-test.f:10 open baked FMT and SOURCE. Under the boundary seal each exits 84. Acceptance: checker-surface.f and both rows are gone, AOT-WL-RECORD calls CHECKER-RESOLVE:WL-PROBE, the two tests open their own packages and pass. Design: .jj-ws.noindex/jerry-seal/.out/seal-design.md. Depends: none. Claim: agent=worker workspace=.jj-ws.noindex/seal-owner.
