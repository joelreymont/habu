---
title: "Infer alloc: class contract table"
status: active
priority: 1
issue-type: task
created-at: "\"2026-07-22T09:43:30.659519+02:00\""
---

Why this exists:
eight allocation classes are named but their owner, lifetime, alignment, access, synchronization, cleanup, and accounting contracts are not executable data.

Required result:
encode the class table as checked records consumed by planners and benchmarks.

Done when:
every first-release class has exactly one complete record; missing or conflicting fields reject; no backing policy is selected yet.

Expected touch points: new maki/infer/allocation-class.f, focused test, FILEMAP.md.
Smallest check: focused class-table test.
Prerequisites: none.
Owned result: allocation class contract only.
Claim: agent=allocclass workspace=.jj-ws/habu-infer-alloc-class-b2201920 machine=spark.
