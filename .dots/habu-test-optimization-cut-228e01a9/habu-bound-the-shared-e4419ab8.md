---
title: Bound the shared build cache
status: closed
priority: 2
issue-type: task
created-at: "2026-09-30T15:12:49.898950+02:00"
closed-at: "2026-09-30T15:13:06.418035+02:00"
close-reason: Landed d9f04485; test/fixture-cache.f dissolved into lib/build-cache.f. Fable review accepted; hb-build-aot-cache rc 67 -> 0. Native gate 504/504 rc 0, 281.2 s wall, 1984 s pooled at load 78-105 (gated at d4d16008, pushed as d8867baf after rebasing over a dots-only commit).
---

Problem: ~/.cache/habu-build grew without bound (1495 entries in the census): hb-build artifacts, the object cache and the gate's keyed images each had their own or no retention, and test/fixture-cache.f duplicated it. Acceptance: one retention in lib/build-cache.f dates hits (BUILD-CACHE:USED) and prunes each family on publish (RETAIN-SECONDS), claiming entries by rename so concurrent pruners never remove one entry twice; hb-build publishes by rename. Verify: build-cache tests including concurrent prune; hb-build-aot-cache passes; full native gate.
