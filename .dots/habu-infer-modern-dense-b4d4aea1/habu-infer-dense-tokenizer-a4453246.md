---
title: "Infer dense: tokenizer and special tokens"
status: open
priority: 1
issue-type: task
created-at: "2026-07-22T09:41:52.435129+02:00"
blocks:
  - habu-infer-dense-pin-36c8e45c
---

Why this exists:
the pinned modern model needs exact tokenizer and special-token semantics, while chat templates must stay outside the engine.

Required result:
load and validate the pinned tokenizer assets, encode/decode fixtures, and expose explicit BOS, EOS, and stop identifiers.

Done when:
public reference prompts round-trip and token identifiers match the trusted tokenizer; missing/duplicate special tokens reject; no chat-template policy enters core runtime.

Expected touch points: new model-family tokenizer module/test and fixture.
Smallest check: focused tokenizer parity test.
Prerequisites: pin product checkpoint.
Owned result: tokenizer assets and special-token semantics only.
Claim: unassigned.
