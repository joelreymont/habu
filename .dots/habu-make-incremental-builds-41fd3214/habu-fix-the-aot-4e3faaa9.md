---
title: Fix the AOT link input race
status: open
priority: 1
issue-type: task
created-at: "2026-10-26T11:28:42.568354+02:00"
---

Joel 2026-10-26. The AOT link session's consumed rows never reach the product manifest (aot-builder.f:117-119 under NDEF-CACHE:WITH-QUIET resets NFILE-INPUTS; the manifest is written earlier, native-definition-cache.f:999-1010): edit a linker source A->B during a build, restore A, and the next build hits a B-linked artifact. Fix: LOAD-LINK-IN-SCOPE appends NFILE-INPUTS:BYTES$ to the HABU_AOT_INPUTS file; move tools/hb-build-producer-race-test.f:41-43's edit before the maker so the test catches it. The builder-image window (aot-builder-image.f:34-50) is a waiting case in habu-simplify-the-build-1af55694. Same lane: delete the stale body-reuse paragraph docs/compilation.md:118-121. Rest of the review (chain-plan.f CWD$, duplicate prefix recorder, double maker hash) when those files are next touched.
