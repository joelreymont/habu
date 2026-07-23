---
title: "BPE: verify GPT-2 artifact identity"
status: open
priority: 1
issue-type: task
created-at: "2026-07-23T09:48:29.643971+02:00"
blocks:
  - habu-bpe-factor-full-62bbd484
  - habu-bpe-own-parity-00afdbd5
---

Problem: compact GPT-2 tables rely on two external facts that are only prose today: the 256 `bytes_to_unicode` tokens map to the pinned encoder identifiers, and the token created by `vocab.bpe` rank `r` has encoder identifier `256+r`. Generating from those assumptions without checking `encoder.json` would preserve a hidden authority gap.

Required result: add checked tool library and entry files under `tools/bpe/` that reopen package `BPE-FULL` privately and verify the hash-pinned `encoder.json` against the factored `vocab.bpe` parse. The command takes explicit encoder and vocab paths after `--`, verifies the exact sizes and SHA-256 values published by `BPE-PARITY`, parses strict JSON through `JR`, and accounts for exactly 50,257 unique key/id pairs. For ids 0..255, each decoded token string must resolve to exactly one canonical byte token and build the byte-to-real-id table. For ids 256..50255, the token string must resolve to the parser token at internal id `256+rank`. The only remaining entry is exact string `<|endoftext|>` at id 50256. Missing, duplicate, fractional, out-of-range, escaped-equivalent, wrong byte token, wrong rank, wrong special token, trailing JSON, size, digest, or vocab mismatch rejects by a package-owned named error. No network access, live BPE installation, public query, or persistent mutable authority.

Prerequisites: `habu-bpe-factor-full-62bbd484` and `habu-bpe-own-parity-00afdbd5`. Owned result: authenticated encoder/rank correspondence and the private verified byte-id table only. It does not select compact merges or render source.

Acceptance: a real run over the pinned GPT-2 artifacts reports one exact success row; synthetic checked fixtures mutate every identity class above and fail; input ordering does not affect JSON object verification; duplicate/missing accounting is mutation-sensitive; the verifier shares the production vocab parser rather than copying it. Files: `tools/bpe/gpt2-verify-lib.f`, focused test, CLI entry, manifests, `FILEMAP.md`. Smallest owning-path check: swap two real encoder ids in a copied artifact, retain valid JSON, and prove the production CLI rejects before generation. Also run exact typed-local, package, trust, host, and file-map checks. Claim: unassigned.
