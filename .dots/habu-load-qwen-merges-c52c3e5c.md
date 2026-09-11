---
title: Load Qwen merges
status: open
priority: 2
issue-type: task
created-at: "2026-07-30T00:55:49.900008+02:00"
blocks:
  - habu-infer-dense-pin-36c8e45c
  - habu-own-model-asset-c6f938e4
  - habu-build-byte-bpe-b915f751
---

Why: Qwen merges.txt has 151387 ordered rows and needs its own authenticated transaction. Interface: package-private QWENTOK:LOAD-MERGES threads MODEL-ASSET workspace and BPE builder, verifies the pinned file once, and adds exactly 151387 ranks in file order. Owner: Qwen merge intake only. Production red: no authenticated Qwen merge table exists. Acceptance: exact first/last/count, duplicate/malformed/reordered/mutated/short-storage and close failures preserve owners. Forbidden: vocabulary, added tokens, encode/decode, alternate rank, fallback file, version, or compatibility path. Smallest owning check: bin/hb --load maki/infer/qwen-merges-test.f.
