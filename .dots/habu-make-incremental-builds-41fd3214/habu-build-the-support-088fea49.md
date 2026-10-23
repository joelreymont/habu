---
title: Build the support image through the cache
status: open
priority: 2
issue-type: task
created-at: "2026-10-22T14:59:32.111952+03:00"
---

Problem: the support-image child (tools/native-build-image.f BUILD$) compiles the driver and the whole native compiler from source at tier 1 with no definition cache: 11.8-12.0 s of an 85 s cold engine build, and again after any tools/ edit; the product build then compiles the same compiler sources a second time. Evidence: `hb-build --engine` builds timed with `/usr/bin/time -l`. Acceptance: the support build loads through the per-definition cache in the same root, so a tools/ comment edit rebuilds the support image recompiling only changed definitions; the support image bytes equal a cold build's; measure cold and tools-edit wall before/after. Files: tools/native-build-image.f, tools/native-build-core.f, tools/hb-build-lib.f. Verify: engine build after a comment edit in a tools/ compiler file, cold engine build, two-generation. Depends: none. Claim: unassigned.
