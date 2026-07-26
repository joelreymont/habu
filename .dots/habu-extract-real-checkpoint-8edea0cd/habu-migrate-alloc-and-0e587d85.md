---
title: Migrate alloc and payload suites to provider
status: open
priority: 2
issue-type: task
created-at: "2026-07-26T22:41:19.107065+02:00"
blocks:
  - habu-create-gpt2-fixture-63b55c1c
---

gpt2-alloc-test.f and gpt2-payload-test.f consume GPT2-FIXTURE; local pinned-fact copies deleted in the same commit, proven gone by boundary-aware sweep. Acceptance: both suites rc=0 through the provider; sweep table; diff lints clean.

Amended (codex preflight 3): this leaf explicitly OWNS removal of the payload suite's presence-gated real execution and publication of one checked GPT2PAY:REAL word containing the existing LOAD/PREPARE/CHECK/COMMIT/payload/dispose leg - the fail-closed entry's defined interface. Without this the entry would edit the same file or copy the leg.
