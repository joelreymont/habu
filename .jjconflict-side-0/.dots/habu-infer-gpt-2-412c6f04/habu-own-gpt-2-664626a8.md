---
title: Own GPT-2 tokenizer
status: closed
priority: 1
issue-type: task
created-at: "2026-07-29T23:22:19.676884+02:00"
closed-at: "2026-08-04T14:26:33.238381+02:00"
close-reason: Landed model-owned GPT-2 tokenizer at cd95673fd996dec8e18168a44b2f17445d2d35a5; root, Claude, and fresh destruction accepted; exact model/generate/CLI/service, full Maki, PTX, canonical, and ownership gates green; workspace and lane temp roots cleaned.
---

Why: production generation loads GPT-2 BPE tables and all encode/decode/generation workspaces into package globals, so two `GPT2:model` values share mutable state and `GPT2:CLOSE` owns none of it. Result: hard-cut that singleton into one checked cell allocation owned by the existing `GPT2:model` record. `GPT2:OPEN ( FS:path -- result<GPT2:model,n> )` authenticates the exact pinned vocabulary and merges once, builds and seals the tokenizer block, and publishes the model only after weights and tokenizer are complete. The existing `M-SAVE`/`M-TAKE` boundary carries the block base and allocation extent; it is widened, not duplicated. `GPT2:CLOSE` releases the block exactly once. Move generation into package `GPT2`; callers use `GPT2:OPEN` and `GPT2:GENERATE`, and package `GPT2-GEN` disappears with no alias. All BPE table, parse, encode, decode, identifier, logits, path, cursor, and counter state is explicit in the model-owned block or call locals. The BPE algorithm stays single and package-private; no BPE or tokenizer owner type is published.

Owner: GPT-2 tokenizer/generation state, its model-record fields, and the atomic deletion of the old singleton implementation only. Production checkpoint: two real `GPT2-GEN:OPEN` calls currently use the same table/workspace addresses through the production load path. Acceptance: two real models own distinct tokenizer blocks, generate interleaved in both close orders, and preserve exact pinned identifiers and `GPT2-REFERENCE:REAL-BYTES$`; pinned prompt and Unicode encode/decode fixtures remain exact; empty, over-capacity, one-byte-short output, mutated vocabulary, mutated merges, allocation refusal, model refusal, and EOS return the model when one was published, preserve caller output, and leak no host mapping, SAFET owner, GPU buffer, or CUDA session. Opening authenticates each tokenizer asset once; every construction failure releases the unpublished block. The exact current CLI and framed service behavior remains, and the old `GPT2-GEN:*`, `BPF-*`, `BPR-*`, singleton BPE storage, old BPE tests/data used only by that surface, and obsolete suite enrollment are absent. Smallest owning checks: `bin/hb --load maki/infer/gpt2-generate-test.f -- gpt2-model` and `bin/hb --load maki/infer/gpt2-cli-device-test.f -- gpt2-model` on DGX Spark.

Forbidden: `MODEL-ASSET`, `GPT2TOK`, public `BPE:builder`/`BPE:codec`, `LINEAR-HANDLE`, scoped borrow, new `TRUSTED:` or `CAST:` site, second BPE algorithm, package-global mutable tokenizer/generation state, callback, BOS/EOS policy change, fallback asset, version, compatibility alias, manifest, lint, framework, or special result type.

Claim: agent=codex-gpt2-token workspace=.jj-ws/habu-own-gpt-2-664626a8
