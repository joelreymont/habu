---
title: Combine direct AOT regression subjects
status: active
priority: 2
issue-type: task
created-at: "2026-10-21T04:18:16.187921+03:00"
---

tools/hb-build-aot-image-test.f currently builds two direct compatibility applications to check ZIP process support and odd25-byte application task-user ceiling, duplicating the same direct-builder boundary. Load ZIP before allocating the25-byte app slot in the first subject, retaining the odd ceiling, then run that product through saved CLI and direct compatibility builds. Keep the distinct second ZIP source saved-maker warm hit/report/runtime check, remove its duplicate direct build. Measure actual row CPU/wall before/after; preserve rooted source restore and all report/output assertions; independent Astra review.
