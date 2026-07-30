---
title: Delete unused host model path
status: active
priority: 1
issue-type: task
created-at: "2026-07-29T21:09:42.368678+02:00"
blocks:
  - habu-pin-gpt-2-cdb5cfe0
  - habu-delete-model-config-1c71a13e
---

Problem: GPT2LOAD, WSTORE, and MODELPROV form an unused host-resident model path. No product caller consumes any of them, while their duplicate model, storage, provenance, version, and dtype authority blocks the direct SAFET-to-GPT2DEV path. Result: delete the three packages, their shared checkpoint fixture, all six package tests, their suite entries, every package-specific trust row and refinement seed, and stale STATUS text. Keep SAFET parsing/mapping, GPT2TENSOR's tensor catalog, MDLCFG, and GPT2PIN. Owner: complete retirement of GPT2LOAD, WSTORE, and MODELPROV; no replacement package is created. Production red: exact source-reference census finds zero non-test consumers and the full suite still loads the dead path. Acceptance: no GPT2LOAD, WSTORE, or MODELPROV definition, suite entry, trust row, or refinement seed remains; SAFET, GPT2TENSOR, MDLCFG, GPT2PIN, reference, trust, package, native fixpoint, and full Maki tests pass. Exact deletion set: `maki/infer/gpt2-load.f`, `maki/infer/weight-store.f`, `maki/infer/model-provenance.f`, `maki/infer/gpt2-checkpoint-fixture.f`, `maki/infer/gpt2-prepare-test.f`, `maki/infer/gpt2-mapped-test.f`, `maki/infer/gpt2-copy-test.f`, `maki/infer/gpt2-payload-test.f`, `maki/infer/weight-store-test.f`, and `maki/infer/model-provenance-test.f`. Exact edits: `maki/test.f`, `maki/test-core.f`, `tools/refine-lint-core.f`, `TRUSTED.md`, `STATUS.md`, and the stale WSTORE comment in `maki/infer/gpt2-tensor-test.f`. Forbidden: replacement host model/store/provenance type, SAFET surface deletion, new test or lint, manifest, schema, version, compatibility path, fallback, or unrelated cleanup. Smallest owning check: native full Maki suite after the deletion. Claim: agent=claude-delete-host-model workspace=.jj-ws/habu-delete-unused-model-40bb32df.
