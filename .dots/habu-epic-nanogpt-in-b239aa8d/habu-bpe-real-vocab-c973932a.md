---
title: BPE real-vocab loading and tiktoken parity
status: active
priority: 1
issue-type: task
created-at: "\"2026-07-20T22:29:35.792619+02:00\""
---

Follow-up named by the BPE close reason (habu-bpe-tokenizer-gpt-37d7f243, landed 815f6437): parity was proven only against a clean-room Python reference with a synthetic vocab because tiktoken and the real GPT-2 vocab files were absent from the box. Deliverables: (1) obtain the real GPT-2 artifacts (encoder.json + vocab.bpe, 50257 tokens = 256 bytes + 50000 merges + <|endoftext|>) with recorded provenance/hashes; decide committed vs runtime-loaded per the recorded DATA-budget rule (full vocab likely runtime-loaded from a checked file) and measure the budget either way. (2) A validated loader for the real vocab+merges (bounds, bijection where required, named E-codes, ready-state gating - the landed tokenizer discipline). (3) Parity fixtures with exact token-id sequences from the reference encoder on known strings, extended beyond ASCII: the landed pre-split matcher is exact for pure ASCII and diverges on bytes >= 0x80 - close that divergence class by implementing the GPT-2 pattern's unicode letter/number classes over UTF-8 bytes honestly, or record a measured, principled boundary with fixtures proving where behavior differs; NO silent approximation. (4) Round-trip and exact-id proofs red-first; encode/decode of a real multi-byte corpus sample. Territory: maki/examples/nanogpt/bpe*.f + fixtures + tests. Independent of the type chain and capacity/attention work.

Claim: agent=tiktok workspace=.jj-ws/fable-tiktok machine=spark (owns maki/examples/nanogpt/bpe*.f + fixtures + tests + registration)
