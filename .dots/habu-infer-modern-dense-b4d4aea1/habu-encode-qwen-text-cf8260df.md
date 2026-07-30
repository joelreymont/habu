---
title: Encode Qwen text
status: open
priority: 1
issue-type: task
created-at: "2026-07-29T23:22:20.308811+02:00"
blocks:
  - habu-infer-dense-tokenizer-a4453246
  - habu-infer-dense-host-4c9152ad
---

Why: Qwen asset construction and its model-specific text splitting are separate implementation outcomes. Result: extend QWENTOK with exact pinned pre-tokenization, byte mapping, added-token handling, ENCODE, and DECODE over the already-open tokenizer owner. The regular expression and special flags match the pinned assets exactly; generation stop and chat-template policy stay outside. Dependency: the Qwen tokenizer asset owner. Owner: QWENTOK text codec only. Production red: the opened Qwen vocabulary cannot encode the 201-byte reference prompt. Acceptance: the prompt yields 39 exact identifiers and round-trips; Unicode, whitespace, contractions, all added tokens, unknown ids, and short buffers match the pinned tokenizer while two owners interleave. Forbidden: tokenizer.json, second BPE, implicit chat template, tokenizer stop policy, fallback, global workspace, version, or compatibility mode. Smallest owning check: bin/hb --load maki/infer/qwen-tokenizer-test.f.
