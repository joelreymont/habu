---
title: Load Qwen added tokens
status: open
priority: 2
issue-type: task
created-at: "2026-07-30T00:55:50.034431+02:00"
blocks:
  - habu-infer-dense-pin-36c8e45c
  - habu-own-model-asset-c6f938e4
---

Why: tokenizer_config.json owns Qwen's exact added-token identifiers and flags, separate from byte-BPE tables. Interface: package-private QWENTOK:LOAD-ADDED threads MODEL-ASSET workspace and tokenizer builder, authenticates the pinned file once, and validates exactly 22 identifiers, byte strings, special flags, and no overlap. Owner: Qwen added-token intake and valid-count derivation only. Production red: added tokens are unvalidated. Acceptance: exact rows/count plus missing/extra/duplicate/flag/string/id/mutation/short-storage and close failures preserve owners. Forbidden: stop policy, merges, base vocabulary, fallback config, version, or compatibility path. Smallest owning check: bin/hb --load maki/infer/qwen-added-test.f.
