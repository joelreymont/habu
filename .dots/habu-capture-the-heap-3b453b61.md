---
title: Capture the heap image sparse and size tables to fill
status: active
priority: 2
issue-type: task
created-at: "\"2026-09-15T18:52:59.710300+03:00\""
---

Problem: the captured heap region of the pinned engine is 3.9 MB, 60% zero bytes, because every prefix table (checker registries, signature pools, index tables) is captured at declared capacity rather than fill. Acceptance: the image stores no zero runs (page-granular sparse blob restored onto anonymous zero pages) and tables that track a fill are captured to their fill; startup restore cost unchanged or lower; image size reported before/after. Files: src/habu/aot-lib.f (EMIT-DATA-BLOB / restore), src/habu/aot-capture.f, src/habu/snap-lib.f. Verify: zero-byte census of bin/hb; bin/hb --load test/run.f. Depends: none. Ownership: AOT image. Claim: agent=hazel-table-fill workspace=.jj-ws/hazel-table-fill.


Parked 2026-09-16 (release first): the census tool landed (tools/data-table-census.f, commit d75e804d); the seam scrub (ARENA-SNAP-BOOT in src/core/checker.f, NORET-BOOT and SEEN-BOOT scrubs, measured DATA image -156,115 bytes, engine -196,608) sits UNCOMMITTED and undescribed in .jj-ws/hazel-table-fill working copy ca408f73, verified by one build only; needs the A/B fixpoint pair and the five named fixtures before landing. The bigger win is the run-row overhead, now dot habu-merge-short-zero.
