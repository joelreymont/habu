---
title: Delete Maxima patch overlays
status: closed
priority: 1
issue-type: task
created-at: "\"2026-04-01T22:06:02.038272+02:00\""
closed-at: "2026-04-01T22:26:03.009036+02:00"
close-reason: done (early patch file deleted; maxima-post-load reduced to bootstrap-only search-root wiring; validation by rg and code inspection; build remains blocked only by unrelated baseline compile errors)
---

Problem: loader still injects semantic Maxima patch files and rewrites. Acceptance: no load path calls lib/maxima-early-patches.lisp and no semantic loader rewrites remain. Files: lib/maxima-early-patches.lisp:1-17, lib/maxima-loader.lisp:28-29,161-171, lib/maxima-post-load.lisp:14-109. Verify: rg -n 'maxima-early-patches|semantic override|quotient' lib/maxima-*.lisp and load smoke. Blockers: none.
