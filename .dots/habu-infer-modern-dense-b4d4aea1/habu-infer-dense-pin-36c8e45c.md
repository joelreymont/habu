---
title: Pin Qwen2.5 artifacts
status: open
priority: 1
issue-type: task
created-at: "2026-07-22T09:41:52.429028+02:00"
blocks:
  - habu-infer-engine-64-02416606
---

Why: the second model must be one immutable checkpoint, not an unspecified family or moving download.

Result: package QWENPIN is data only: it owns revision `a09a35458c702b33eeacc393d103063234e8bc28` of `Qwen/Qwen2.5-7B-Instruct` and the exact basename, byte length, and SHA-256 constants below:

- `config.json`, 663, `7463bb0ea78315365e6c6b74de4e73bbcc8359dfb0c5a737584e077d42c0b03c`
- `merges.txt`, 1671839, `599bab54075088774b1733fde865d5bd747cbcc7a547c5bc12610e874e26f5e3`
- `model.safetensors.index.json`, 27752, `624bf7c47cd12468fdc16e38a47cf4f19e0415b859a223ba3c027eed2f0e1028`
- `model-00001-of-00004.safetensors`, 3945441440, `a1333e6293854747c481288ea83b348226af178dd565c49b6f9495ba1966aba7`
- `model-00002-of-00004.safetensors`, 3864726352, `f5d25a2772cb825164a2a2c0fb6d51a87e282abf21e4dd75bc5cfb3cd0ea6185`
- `model-00003-of-00004.safetensors`, 3864726424, `8efdec4c1bc12317ae1a38dc42b595ce777738a64deea3fcb8a0a91381bcdfd5`
- `model-00004-of-00004.safetensors`, 3556377672, `1a72d403cdf0c1ec3cb7f289f17b394a01e64394c2e9b3c0f94dbce3faf879bd`
- `tokenizer_config.json`, 7305, `5b5d4f65d0acd3b2d56a35b56d374a36cbc1c8fa5cf3b3febbbfabf22f359583`
- `vocab.json`, 2776833, `ca10d7e9fb3ed18575dd1e277a2579c16d108e32f27439684afa0e10b1440910`

`tokenizer.json` is not a runtime input because it duplicates `vocab.json` and `merges.txt`; `generation_config.json` is not a runtime input. This product arm explicitly chooses BOS and pad 151643 and the ordered stop identifiers 151645 then 151643; final reference acceptance proves that policy. Unrelated files are ignored. Downstream consumers open their exact basename and compute SHA-256 over the same bytes they parse or stage; small assets are parsed from their transient build buffer, while shard bytes remain provisional until their digest matches. No raw verified-root value or separate preflight exists. The checkpoint is Apache-2.0. Add no policy owner, manifest, root owner, schema, download client, directory census, alternate filename, fallback revision, selector, generated artifact, or pack.

Owner: new `maki/infer/qwen-pin.f` artifact and token constants plus focused data test only. Production red: no exact Qwen asset or stop identity is declared. Acceptance: the focused test hashes the authenticated local snapshot and matches every basename, length, digest, revision, and explicit token-policy constant; mutating any constant fails that test. Missing, renamed, truncated, appended, one-byte-mutated, and single-open behavior belongs to downstream consumers. Smallest owning check: `bin/hb --load maki/infer/qwen-pin-test.f`. Claim: unassigned.
