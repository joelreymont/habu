---
title: Migrate alloc and payload suites to provider
status: closed
priority: 2
issue-type: task
created-at: "2026-07-26T22:41:19.107065+02:00"
closed-at: "2026-08-02T16:43:02.283931+02:00"
close-reason: authoritative ancestor 5b0ebb070a5b8ef7c04e2d28772421f796b686c6 deleted the unused GPT2LOAD/GPT2TX/WSTORE/MODELPROV host architecture and suites; retaining the task would resurrect deleted architecture.
blocks:
  - habu-create-gpt2-fixture-63b55c1c
---

gpt2-alloc-test.f and gpt2-payload-test.f consume GPT2-FIXTURE; local pinned-fact copies deleted in the same commit, proven gone by boundary-aware sweep. Acceptance: both suites rc=0 through the provider; sweep table; diff lints clean.

Amended (codex preflight 3): this leaf explicitly OWNS removal of the payload suite's presence-gated real execution and publication of one checked GPT2PAY:REAL word containing the existing LOAD/PREPARE/CHECK/COMMIT/payload/dispose leg - the fail-closed entry's defined interface. Without this the entry would edit the same file or copy the leg.
