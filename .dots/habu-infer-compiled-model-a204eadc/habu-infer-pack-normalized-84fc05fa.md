---
title: "Infer pack: normalized model config"
status: open
priority: 1
issue-type: task
created-at: "\"2026-07-22T09:40:20.799136+02:00\""
blocks:
  - habu-add-generic-bounded-359c0944
  - habu-add-shared-inference-0dad1107
  - habu-integrate-reentrant-json-34850f2f
---

Why this exists:
runtime code must not repeatedly interpret architecture-specific Hugging Face JSON.

Required result:
parse the selected source configuration into a versioned normalized decoder config with explicit dimensions, dtypes, tokenizer identity, positional scheme, normalization, activation, and special tokens.

Done when:
supported GPT-2 and modern-dense fixtures normalize canonically; missing, inconsistent, overflowed, and unsupported fields reject named with no partial record.

Expected touch points: new maki/infer/model-config.f, new maki/infer/model-config-test.f, FILEMAP.md.
Smallest check: bin/hb --load maki/infer/model-config-test.f.
Prerequisites: landed JSON reader and safetensors loader; habu-add-generic-bounded-359c0944; habu-add-shared-inference-0dad1107; habu-integrate-reentrant-json-34850f2f.
Owned result: normalized config only.
Claim: released.

Stale claim reconciled (2026-07-25): the peer orchestrator confirmed this lane dead in blackboard message 20260724-190033.997-codex-30ac on channel general, which states "I confirm the four old claims are stale: no live worker owns safetensors d3d3a8a6, normalized config 84fc05fa, manifest 27c1030c, or GPT-2 binding f2ed655d", and undertook to release them in the next metadata wave. The former modelconfig workspace .jj-ws/habu-infer-pack-normalized-84fc05fa is evidence only. This contract is being superseded by the rev-4 inference leaf redesign posted as 20260724-191041.846-claude-7d24 on channel general, whose correction 2 replaces the flat four-enum configuration with a unified payload ENUM for architecture-specific validated config; do not implement from the description above until that redesign has replaced or re-frozen it.
