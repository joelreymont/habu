---
title: Extract real-checkpoint fixture provider
status: closed
priority: 2
issue-type: task
created-at: "2026-07-26T22:29:05.192521+02:00"
closed-at: "2026-08-02T16:43:20.904628+02:00"
close-reason: authoritative ancestor 5b0ebb070a5b8ef7c04e2d28772421f796b686c6 deleted the unused GPT2LOAD/GPT2TX/WSTORE/MODELPROV host architecture and suites; retaining the task would resurrect deleted architecture.
blocks:
  - habu-create-gpt2-fixture-63b55c1c
  - habu-migrate-bind-and-ea0d7c44
  - habu-migrate-alloc-and-0e587d85
---

Why: codex blind review - the checkpoint path, GPT-2 124M configuration, expected census, sha, and exact byte count are now duplicated across gpt2-bind-test, gpt2-check-test, gpt2-alloc-test, and gpt2-payload-test; four copies of pinned facts drift independently (the payload suite already had to respell the configuration because GPT2TX publishes no fixtures). Behavior: one package-owned real-checkpoint provider (test-support package; path resolution, sha, byte count, census constant, 124M configuration values, presence probe) consumed by all four suites; every local copy deleted; the provider is the single place the pinned facts live and the fail-closed entry (habu-require-real-checkpoint) reads it. Owner: new test-support package under maki/infer. Acceptance: all four suites green consuming the provider with their local constants gone (boundary-aware sweep proves no duplicate literals remain); diff lints clean. Real pre-change defect: four independent copies of the same pinned facts, measured.


Reshaped 2026-07-26 (codex gate-stop accepted): coordination parent; implementation in habu-create-gpt2-fixture-63b55c1c (provider, package GPT2-FIXTURE, which OWNS the single SHA literal as DIGEST$ — GPT2-REFERENCE:VERIFY-ARTIFACT delegates to GPT2-FIXTURE:VERIFY-PATH in the same commit, one digest authority with the fixture owner minimal), then habu-migrate-bind-and-ea0d7c44 and habu-migrate-alloc-and-0e587d85 (consumer migrations, each deleting its local copies in the same commit).
