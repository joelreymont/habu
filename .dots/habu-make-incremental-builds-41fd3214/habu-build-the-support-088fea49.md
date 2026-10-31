---
title: Build the support image through the cache
status: open
priority: 2
issue-type: task
created-at: "2026-10-22T14:59:32.111952+03:00"
---

Problem: the support-image child (tools/native-build-image.f BUILD$) compiles the driver and the whole native compiler from source at tier 1 with no definition cache: 11.8-12.0 s of an 85 s cold engine build, and again after any tools/ edit; the product build then compiles the same compiler sources a second time. Evidence: `hb-build --engine` builds timed with `/usr/bin/time -l`. Acceptance: the support build loads through the per-definition cache in the same root, so a tools/ comment edit rebuilds the support image recompiling only changed definitions; the support image bytes equal a cold build's; measure cold and tools-edit wall before/after. Files: tools/native-build-image.f, tools/native-build-core.f, tools/hb-build-lib.f. Verify: engine build after a comment edit in a tools/ compiler file, cold engine build, two-generation. Depends: none. Claim: unassigned.

Measured 2026-10-31 at lane 11 (uyzslxrr), no-change warm engine build 46.0 s wall, dot habu-profile-a-warm-9f10659a, report carl-dots/.out/carl-file/s2prof-report.md: warm builds hit the support image (parent residual 0.32 s); cold, the support image is written ~22 s after the 3.6 s startup (was 12 s). tools/native-build-image.f BUILD$ :154, tools/hb-build-lib.f HBB-ENGINE-IMAGE! 1031-1048. Whether the child uses the definition cache was not checked.
