---
title: Keep rigid contracts out of cache failures
status: active
priority: 1
issue-type: task
created-at: "2026-10-16T01:17:02.486593+03:00"
---

With hb-closed a4528b42 built from b86460a3, cold native builds pass but a normal warm self-build dies76 with checker: a captured rigid atom has process-local identity after definition dependency misses. Existing native-build-edit E2E covers the failed production path. Identify the exact word and whether import corrupts a persistent graph or cache contract export asks for a nonportable transient graph. Preserve rigid identity and all public graph refusals; valid source must continue through cold compilation when its contract cannot be reused. Verify the same frozen source cold/warm products and required core gates; independent Astra review before landing.
