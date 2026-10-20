---
title: Fix length-sensitive AOT source loading
status: active
priority: 1
issue-type: task
created-at: "2026-10-16T13:18:21.304640+03:00"
---

The rebuilt 5b93a9ee engine fails AOT-BUILDER:OPEN with E-BOUNDS before object output for the retained maplate.f fixture at a 97-byte path; the same binary and source succeed when the parent directory is one byte shorter or longer. A fresh build cache preserves the failure. Trace the exact source-loader/storage boundary, fix the responsible layer, and preserve equivalent behavior for absolute path lengths. Existing stripped-cache E2E and the retained short direct-builder reproduction are the regression path; require independent Astra review and affected native checks before landing.
