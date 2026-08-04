---
title: "Remove GPT-2 chunk ceiling"
status: closed
priority: 1
issue-type: task
created-at: "\"2026-07-23T09:47:41.006677+02:00\""
close-reason: "Landed as d7699336545151c09df33f057d82110b50b2a518; maki/test.f, gate-stdlib lint-libs including ptx-stdlib/toolchain, and test/run.f passed."
blocks:
  - habu-bpe-install-unicode-3c84e7a1
---

Why: the model-owned tokenizer admits 4,096 input bytes but still has an unrelated 1,024-cell `T-WORK`, so legal single chunks of 1,025 through 4,096 bytes refuse. Result: remove `T-WORK-CAP` and the separate `T-WORK` region. Stage the current raw-byte chunk at `T-OUT + T-OUTN`, merge there in place, and advance `T-OUTN` only by the reduced symbol count. The structural bound is `finalized_ids + current_chunk_bytes <= admitted_input_bytes <= T-ID-CAP`; no second scratch region or larger allocation is needed. Preserve zero per-call allocation, merge order, real-id translation, single final output publication, and the 4,096-byte public limit. A 4,097-byte input still refuses before caller output changes. Raising the scratch cap or keeping duplicate storage is forbidden.

Owner: `maki/infer/gpt2-token.f` tokenizer workspace layout, exact model allocation accounting, and model-owned production tests. Source evidence to adapt, not merge: `182eaeba1f39b89db3e627860ec617f36c19e245`. This result does not own chunk classification, vocabulary data, tokenizer lifetime, or a larger public limit. Production red: real private `GPT2:ENCODE` rejects a 1,025-byte all-Letter input with `E-TOK-CAP`.

Acceptance: real model-owned `GPT2:ENCODE` plus decode round-trip exact single chunks of 1,024, 1,025, and 4,096 bytes; 4,097 refuses before caller output mutation. A 4,096-byte multi-chunk input proves finalized prefix identifiers and later chunk work cannot overlap. Canaries around source and decoded output remain intact. Exact `GPT2:model` allocation accounting removes 1,024 cells and has no unexplained growth. Mutating tail addressing, finalized count stability, or either boundary fails an owning test. Smallest owning-path check: open the pinned model and round-trip a 1,025-byte all-Letter input through the same private `GPT2:ENCODE` path used by `GPT2:GENERATE`. Files: `maki/infer/gpt2-token.f`, `maki/infer/gpt2-model-test.f`, and `maki/infer/gpt2-generate-test.f`. Run focused model/generate, typed-local, package, and canonical gates.
