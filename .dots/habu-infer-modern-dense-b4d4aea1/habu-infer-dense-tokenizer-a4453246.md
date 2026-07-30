---
title: Open Qwen tokenizer
status: open
priority: 1
issue-type: task
created-at: "2026-07-22T09:41:52.435129+02:00"
blocks:
  - habu-infer-dense-pin-36c8e45c
  - habu-load-qwen-vocabulary-f96b28fa
  - habu-load-qwen-merges-c52c3e5c
  - habu-load-qwen-added-23be2db2
  - habu-encode-byte-bpe-46ceac21
  - habu-decode-byte-bpe-1df3e002
---

Why: the three authenticated Qwen tokenizer inputs and reusable byte-BPE operations need one model-specific owner. Result: QWENTOK:OPEN takes and returns MODEL-ASSET:ws and a bounded root span, invokes the vocabulary, merge, and added-token transactions in order, seals one BPE state, computes MAX-TOKEN-BYTES, and publishes VALID-COUNT 151665. It delegates ENCODE and DECODE to BPE; text splitting is added by the next leaf. Every refusal returns the workspace and releases the partial tokenizer builder exactly once. BOS, EOS, pad, chat formatting, and sampling remain outside. Owner: QWENTOK composition, publication, and release only. Production red: the independently validated inputs do not yet form one tokenizer owner. Acceptance: the exact inputs publish once; every component refusal publishes nothing; two owners interleave; release is total; no package-global path, file, table, or work buffer remains. Forbidden: asset parsing, second BPE algorithm, tokenizer.json reader, verified-root value, callback, chat template, stop rule, guessed bound, version, compatibility path, or sampling rule. Smallest owning check: bin/hb --load maki/infer/qwen-tokenizer-test.f. Claim: unassigned.
