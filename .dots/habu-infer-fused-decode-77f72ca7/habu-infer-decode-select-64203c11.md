---
title: "Infer decode: select page transfer path"
status: open
priority: 1
issue-type: task
created-at: "2026-07-22T09:41:52.384485+02:00"
blocks:
  - habu-infer-decode-vector-e5ac69b3
  - habu-infer-decode-tma-834cc287
  - habu-infer-decode-async-8ba85dca
  - habu-infer-m0-schema-27e8ca5d
---

Why this exists:
the runtime needs an empirical transfer choice by page size and context regime, not an architectural guess.

Required result:
run vector, TMA, and asynchronous variants under one idle-machine harness for the supported geometry matrix and record the winning schedule keys with uncertainty.

Done when:
every candidate uses identical inputs and correctness gate; short, medium, and long contexts are represented; unsupported or statistically tied cases remain explicit.

Expected touch points: decode benchmark, canonical GB10 result record, and schedule-key table.
Smallest check: schema validation and repeatable reducer.
Prerequisites: vector-load, TMA, async candidates and M0 benchmark schema.
Owned result: transfer benchmark and schedule selection only.
Claim: unassigned.
