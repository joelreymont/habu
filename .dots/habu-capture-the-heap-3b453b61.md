---
title: Capture the heap image sparse and size tables to fill
status: open
priority: 2
issue-type: task
created-at: "2026-09-15T18:52:59.710300+03:00"
---

Problem: the captured heap region of the pinned engine is 3.9 MB, 60% zero bytes, because every prefix table (checker registries, signature pools, index tables) is captured at declared capacity rather than fill. Acceptance: the image stores no zero runs (page-granular sparse blob restored onto anonymous zero pages) and tables that track a fill are captured to their fill; startup restore cost unchanged or lower; image size reported before/after. Files: src/habu/aot-lib.f (EMIT-DATA-BLOB / restore), src/habu/aot-capture.f, src/habu/snap-lib.f. Verify: zero-byte census of bin/hb; bin/hb --load test/run.f. Depends: none. Ownership: AOT image. Claim: unassigned.
