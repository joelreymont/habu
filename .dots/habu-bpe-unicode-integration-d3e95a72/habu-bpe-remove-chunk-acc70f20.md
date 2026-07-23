---
title: "BPE: remove chunk workspace ceiling"
status: active
priority: 1
issue-type: task
created-at: "\"2026-07-23T09:47:41.006677+02:00\""
---

Problem: `BPE-ENCODE` admits 4,096 input bytes but stages one chunk in a separate 1,024-cell `BPE-WORK`. A legal 1,025-byte Letter run therefore throws `E-BPE-CAP`; the public input contract and its internal workspace disagree.

Required result: remove `BPE-CHUNK-CAP`, `BPE-WORK`, and the independent chunk ceiling. Use the unused tail of the existing 4,096-token staged output buffer as the current chunk work area. The invariant is structural: completed chunks emit no more symbols than source bytes consumed, so `output-count + next-chunk-bytes <= admitted-input-bytes <= 4096`. Keep `BPE-OUT` fixed while one chunk reaches its merge fixpoint, address work symbols relative to that offset, then advance it by the reduced symbol count. Preserve zero allocation, single final caller write, merge order, real-id translation, and the 4,096-byte public limit. Inputs above 4,096 still reject before touching caller output.

Prerequisites: none. Owned result: encode workspace layout and its bound only. It does not own chunk classification, vocabulary data, tokenizer instances, or a larger public input limit.

Acceptance: production `BPR-ENCODE` plus `BPR-DECODE` round-trip exact single chunks of 1,024, 1,025, and 4,096 bytes; 4,097 rejects with `E-BPE-CAP`; canaries around source, staged ids, decoded bytes, and internal buffer remain intact; a too-small caller output is byte-identical after rejection; multi-chunk inputs prove earlier output and later work cannot overlap. Mutating tail addressing, output-count stability, or either boundary fails a focused test. Measure DATA bytes before/after and require removal of the duplicate 1,024-cell workspace with no unexplained growth. Files: `maki/examples/nanogpt/bpe.f`, focused production tests, `FILEMAP.md`. Smallest owning-path check: round-trip a 1,025-byte all-Letter input through the public real-vocabulary encoder and decoder with canaries. Also run exact typed-local, package, host, and file-map checks. Claim: agent=bpe_capacity workspace=.jj-ws/habu-bpe-remove-chunk-acc70f20.
