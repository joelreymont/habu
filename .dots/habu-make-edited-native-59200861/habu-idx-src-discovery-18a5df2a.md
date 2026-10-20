---
title: Index source discovery intervals
status: active
priority: 1
issue-type: task
created-at: "2026-10-15T16:40:44.508069+03:00"
---

Measured frozen artifact-hit invocation spends about 3.4 seconds CPU rediscovering support closures; a diagnostic pass takes 3.96 seconds wall across hb-build, aot-builder and aot-build. DISCOVER:SD-R-CONTEXT-BETWEEN scans all declaration rows at every loader interval. Use the existing source-file identity and row lifecycle to visit only rows in the requested file and offset interval, preserving actual include order, package/family/retirement semantics and fail-closed discovery. Prefer extending an existing file chain/index over duplicate state or another source cache. Existing real source-discovery/event-closure tests and the exact frozen closure/artifact-hit measurement are acceptance; full native gate belongs to the integrated core batch. Evidence: /Users/joel/.cache/tmp/habu-small-build.yjwt1l/cost/receipt.md. This alone does not remove the separately measured 3.53 seconds CPU driver load or establish a subsecond build.
