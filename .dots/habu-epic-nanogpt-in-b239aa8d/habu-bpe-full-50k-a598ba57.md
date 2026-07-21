---
title: "BPE: full 50k-merge load (rank hashmap)"
status: active
priority: 2
issue-type: task
created-at: "\"2026-07-20T23:03:34.674885+02:00\""
blocks:
  - habu-bpe-real-vocab-c973932a
---

Loose end from the real-vocab BPE landing (808d6c99): the landed engine's BPE-RANK is a linear scan (O(merges) per pair) with BPE-MAX-MERGE=512, so loading the full 50000 GPT-2 merges is O(N^2)-unusable and overflows the cap. Give the engine a rank lookup keyed by pair (hashmap or sorted table + binary search - measure both), raise the merge capacity to hold the full table (measured DATA budget), and load the full runtime-fetched vocab.bpe (fetch-gpt2-vocab.sh, hashes pinned) end-to-end: encode/decode real text against tiktoken full-vocab parity fixtures, throughput measured. Red-first: capacity guard fires at the old cap; parity fixtures fail with the subset-only table where the full table changes the outcome.

Claim: agent=bpe50k workspace=.jj-ws/fable-bpe50k machine=spark (owns the BPE rank-lookup engine upgrade + full-vocab load: maki/examples/nanogpt/bpe*.f + fixtures)
