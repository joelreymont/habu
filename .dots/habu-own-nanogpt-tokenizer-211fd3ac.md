---
title: Own NanoGPT tokenizer instances
status: open
priority: 2
issue-type: task
created-at: "2026-07-21T22:07:52.739943+02:00"
---

maki/examples/nanogpt/tokenizer.f returns a handle over singleton backing tables, so apparent tokenizer instances alias and failed or repeated construction can expose mixed state. Define a package-owned immutable tokenizer value whose vocabulary, byte maps, merge ranks, readiness, and artifact identity are built in staged owned storage and published once after complete validation. Encoding and decoding consume an explicit tokenizer plus caller-owned workspace; no global current tokenizer or fallback tables. Two vocabularies must coexist and interleave, and a failed rebuild must leave an existing tokenizer byte-identical. Integrate the BPE Unicode/scalar and full-vocabulary owners rather than duplicating their algorithms; this dot owns instance identity, storage, construction, and API threading. Add independent vocabularies, interleaved encode/decode, nested calls, malformed/capacity failures, stale handle, artifact swap, cleanup, exact GPT-2 fixtures, and import-no-side-effect tests. Files: NanoGPT tokenizer instance/API and direct callers/tests. Verify BPE/tokenizer/NanoGPT suites, Maki, typed-local/package/host/filemap/dot lints, and full native gate.
