---
title: "Infer pack: normalized model config"
status: active
priority: 1
issue-type: task
created-at: "\"2026-07-22T09:40:20.799136+02:00\""
---

Why this exists:
runtime code must not repeatedly interpret architecture-specific Hugging Face JSON.

Required result:
parse the selected source configuration into a versioned normalized decoder config with explicit dimensions, dtypes, tokenizer identity, positional scheme, normalization, activation, and special tokens.

Done when:
supported GPT-2 and modern-dense fixtures normalize canonically; missing, inconsistent, overflowed, and unsupported fields reject named with no partial record.

Expected touch points: new maki/infer/model-config.f, new maki/infer/model-config-test.f, FILEMAP.md.
Smallest check: bin/hb --load maki/infer/model-config-test.f.
Prerequisites: landed JSON reader and safetensors loader.
Owned result: normalized config only.
Claim: agent=modelconfig workspace=.jj-ws/habu-infer-pack-normalized-84fc05fa machine=spark.
