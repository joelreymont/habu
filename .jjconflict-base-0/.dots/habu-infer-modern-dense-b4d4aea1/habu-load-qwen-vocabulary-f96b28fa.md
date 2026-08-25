---
title: Load Qwen vocabulary
status: open
priority: 2
issue-type: task
created-at: "2026-07-30T00:55:49.782184+02:00"
blocks:
  - habu-infer-dense-pin-36c8e45c
  - habu-own-model-asset-c6f938e4
  - habu-build-byte-bpe-b915f751
---

Why: Qwen vocab.json authentication and entry insertion are independent from merges and added tokens. Interface: package-private QWENTOK:LOAD-VOCAB threads MODEL-ASSET workspace and BPE builder, opens the pinned basename once, verifies length/digest, and adds exactly the declared token-byte/id rows. Owner: Qwen base vocabulary intake only. Production red: no Qwen vocabulary owner exists. Acceptance: exact count, duplicate id/bytes, malformed JSON, mutation, short workspace, and close failure preserve owners. Forbidden: merges, added tokens, encode/decode, second table, fallback file, version, or compatibility path. Smallest owning check: bin/hb --load maki/infer/qwen-vocab-test.f.
