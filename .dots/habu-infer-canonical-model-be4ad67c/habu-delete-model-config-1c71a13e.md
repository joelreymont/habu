---
title: Delete model config version
status: closed
priority: 2
issue-type: task
created-at: "2026-07-29T20:34:14.770343+02:00"
closed-at: "2026-07-30T13:05:59.879071+02:00"
close-reason: Landed and verified at master@origin cf59aca4753c1d41877f4e00f46a8c349f37a166
---

Problem: maki/infer/model-config.f stores, validates, hashes, and publishes a schema version although the product has no installed compatibility contract or old reader; the field is duplicate state and forces speculative version handling into every consumer. Result: remove mcfg.sv, SCHEMA-FIRST, schema validation and folding, E-SCHEMA if no other use remains, and the schema argument from MDLCFG:BUILD; migrate every caller atomically. Current representation only: no default version, legacy arity, alias, upgrade path, or unknown-version branch. Config-key deletion is a separate dependent hard cut. Owner: package MDLCFG and exact callers/tests. Structural red: the owning source contains `FIELD sv n`, `SCHEMA-FIRST`, `V-SCHEMA`, `SCHEMA@`, and schema-key folding, and the real public BUILD callers all pass the same meaningless `1`. Acceptance: old BUILD arity and schema words do not resolve; canonical GPT-2 and Llama configurations build; every remaining semantic-field rejection still throws its exact named code; byte-identical current configurations retain stable keys; model-config, GPT2TENSOR, and extant GPT2LOAD caller suites plus typed-local and package exact-diff gates pass.

Exact write set: `maki/infer/model-config.f`, `maki/infer/model-config-test.f`, `maki/infer/gpt2-tensor-test.f`, `maki/infer/gpt2-prepare-test.f`, `maki/infer/gpt2-payload-test.f`, `maki/infer/gpt2-checkpoint-fixture.f`, and comment-only corrections in `maki/infer/model-provenance.f`. The fixture owns the five bare `BUILD` calls shared by the prepare, mapped, and copy suites. No parser, device loader, compatibility surface, lint, documentation, or unrelated configuration cleanup belongs here.

Claim: agent=claude-model-config workspace=.jj-ws/habu-delete-model-config-1c71a13e
