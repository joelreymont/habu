---
title: check.f fails open on prefix paths
status: open
priority: 2
issue-type: task
created-at: "2026-08-20T12:11:31.997328+02:00"
---

Found by route3-1 (2026-08-20): pointing tools/check.f --source-list at a canonical prefix path (src/core/type-schema.f) exits 0 in 0.6s HAVING CHECKED NOTHING - REQUIRE-KNOWN? (include.f:100) byte-compares, the boot prefix marks its files provided, CHK-DEP-PRELOAD? (check-core.f:1046) silently skips. The ./-prefixed spelling of the same bytes is checked for real. A gate tool exiting 0 having verified zero definitions is fail-OPEN. Fix: CHK-DEP-PRELOAD? refuses by name when every positional is engine-provided. LESSONS.md:784 only half-records this (expects a noisy E-UNDEFINED; reality is silence).
