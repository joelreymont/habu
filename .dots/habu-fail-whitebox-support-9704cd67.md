---
title: Fail whitebox support fixture before setup
status: active
priority: 2
issue-type: task
created-at: "2026-10-21T04:36:33.243476+03:00"
---

Retained-cache whitebox row still costs32.65CPU. Its ordinary child-failure fixture appends a missing FILE-SIZE access after native-build-support.f has loaded the complete compiler, although the tested boundary is child exit67/no published supplier. Measure existing --timeout-only cases and replace only the copied support file with a minimal real filesystem-failure program (require public fs then missing FILE-SIZE), avoiding unrelated compiler initialization before the same failure. Keep real provider/build child, exact timeout/exit67/no-product assertions and private cache/root. Adopt only if measured CPU falls and responsible failure boundary remains equivalent; independent review.
