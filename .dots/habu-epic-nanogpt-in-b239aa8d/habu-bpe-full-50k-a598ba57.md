---
title: "BPE: full 50k-merge load (rank hashmap)"
status: closed
priority: 2
issue-type: task
created-at: "\"\\\"2026-07-20T23:03:34.674885+02:00\\\"\""
closed-at: "2026-07-21T07:41:15.413528+02:00"
close-reason: "Landed in stack cb1e4cae: full 50000-merge GPT-2 vocab load. Rank lookup measured head-to-head on the real query stream (21792 lookups x200, GB10): hashmap 51ns vs binary-search 270ns - hashmap chosen (5.3x, and O(1) incremental build fits BPE-MERGE+). Caps 512->50000 with measured DATA budget (~4.4MB static + ~6MB full-load-only); two more O(N^2)/O(N) walls fixed en route (BPR-MID-OK? presence-set, BPR-REAL>INT direct-index inverse). New bpe-full.f resolves all 100000 child refs through an FNV string map and installs the full table; proven: exact tiktoken 0.13.0 parity on all committed fixtures, real-text round-trip, 753k tokens/sec. Red-first: capacity guard fires at the old cap; the tokenization fixture proves subset-table and full-table ids DIVERGE exactly as pinned. Presence-gated on the fetched artifacts; hermetic gate path unchanged"
---

Loose end from the real-vocab BPE landing (808d6c99): the landed engine's BPE-RANK is a linear scan (O(merges) per pair) with BPE-MAX-MERGE=512, so loading the full 50000 GPT-2 merges is O(N^2)-unusable and overflows the cap. Give the engine a rank lookup keyed by pair (hashmap or sorted table + binary search - measure both), raise the merge capacity to hold the full table (measured DATA budget), and load the full runtime-fetched vocab.bpe (fetch-gpt2-vocab.sh, hashes pinned) end-to-end: encode/decode real text against tiktoken full-vocab parity fixtures, throughput measured. Red-first: capacity guard fires at the old cap; parity fixtures fail with the subset-only table where the full table changes the outcome.

Claim: agent=bpe50k workspace=.jj-ws/fable-bpe50k machine=spark (owns the BPE rank-lookup engine upgrade + full-vocab load: maki/examples/nanogpt/bpe*.f + fixtures)
