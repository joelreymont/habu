---
title: Own GPT-2 tokenizer
status: open
priority: 1
issue-type: task
created-at: "2026-07-29T23:22:19.676884+02:00"
blocks:
  - habu-own-gpt-2-45d7d1e4
  - habu-pin-gpt-2-cdb5cfe0
  - habu-build-byte-bpe-b915f751
  - habu-encode-byte-bpe-46ceac21
  - habu-decode-byte-bpe-1df3e002
  - habu-own-model-asset-c6f938e4
---

Why: the reusable byte-BPE owner and GPT-2 asset adapter are separate responsibilities. Result: GPT2TOK:OPEN takes and returns MODEL-ASSET:ws with a root ptr u8 plus CAD-NUM:byte-len, authenticates each exact GPT2PIN vocabulary and merges asset once, feeds the BPE builder, and publishes one tokenizer only after table sealing succeeds. It owns exact GPT-2 pre-tokenization and delegates ENCODE and DECODE to BPE; it exports VALID-COUNT 50257, MAX-TOKEN-BYTES, ENCODE, DECODE, and RELEASE. Every result arm returns the workspace. Owner: new maki/infer/gpt2-tokenizer.f composition and asset adapter only. Production red: after BPE extraction, no model adapter can encode the pinned prompt. Acceptance: each asset opens and hashes once; pinned prompt and Unicode fixtures match exact identifiers and bytes; unsafe or overlong root and one-byte-short output return the workspace without publishing a tokenizer; malformed, duplicate, mutated, incomplete, or capacity-swapped assets consume and release the terminal BPE builder, return the workspace, and leak no owner or mapping; two tokenizers interleave; release is total. Forbidden: package-global path or file buffer, second BPE algorithm, callback, BOS or EOS policy, guessed bound, alias, version, compatibility path, or recoverable builder wrapper. Smallest owning check: bin/hb --load maki/infer/gpt2-tokenizer-test.f.
