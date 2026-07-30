---
title: Pin GPT-2 artifacts
status: active
priority: 1
issue-type: task
created-at: "2026-07-29T23:44:27.835265+02:00"
---

Why: GPT-2 config, tokenizer, reference data, and device loader need one exact checkpoint identity. Result: package GPT2PIN is data only and owns openai-community/gpt2 revision 607a30d783dfa663caf39e06633721c8d4cfcd7e plus exact basename, length, and SHA-256 constants: config.json 665 0daed7749b4f02b8f76240d5444551d7b08712dab4d0adb8239c56ba823bb7b4; model.safetensors 548105171 248dfc3911869ec493c76e65bf2fcf7f615828b0254c12b473182f0f81d3a707; vocab.json 1042301 196139668be63f3b5d6574427317ae82f612a97c5d1cdaf36ed2256dbf636783; merges.txt 456318 1ce1664773c50f3e0cc8842619a93edc4624525b728b188a9e0be33b7726adc5. Downstream consumers open their basename and hash the same bytes they parse or stage; no verified-root value exists. Owner: new package GPT2PIN in `maki/infer/gpt2-pin.f`; its focused external test may read `gpt2-model/` but the package exposes data only. Production red: the package and every pin word are undefined while four independent consumers need the same identity. Acceptance: the focused test hashes the authenticated local snapshot and matches every basename, length, digest, and revision constant; mutating any constant fails that test. A missing local snapshot may skip the artifact leg in the aggregate suite but cannot satisfy this dot's acceptance run. Missing, renamed, truncated, appended, one-byte-mutated, and single-open behavior belong to the downstream parser, tokenizer, and staging leaves. Forbidden: manifest, root owner, schema, downloader, directory scan, alternate filename, fallback revision, model selector, generated artifact, pack, version, or compatibility path.

Exact write set: new `maki/infer/gpt2-pin.f` and `maki/infer/gpt2-pin-test.f`, plus the two existing suite inventories `maki/test.f` and `maki/test-core.f`. Smallest owning check: `bin/hb --load maki/infer/gpt2-pin-test.f` with the authenticated local snapshot present; package and typed-local gates run on the same exact diff.

Claim: agent=codex-gpt2-pin workspace=.jj-ws/habu-pin-gpt-2-cdb5cfe0
