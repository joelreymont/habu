---
title: Run GPT-2 CLI for 64 tokens
status: active
priority: 1
issue-type: task
created-at: "2026-07-22T09:41:52.417979+02:00"
---

Why: `GPT2:GREEDY` now advances the real persistent model, but no production command turns prompt bytes into generated text. Result: add one direct CLI, `bin/hb --load tools/gpt2.f -- <model-root> <prompt>`, plus its focused real-device proof. In one process it joins `<model-root>` with `GPT2PIN:MERGES-NAME$`, verifies that file against `GPT2PIN:MERGES-SHA256$`, calls the existing in-place `BPF-LOAD` exactly once, encodes the nonempty prompt through `BPR-ENCODE`, opens `GPT2:model`, feeds every prompt identifier through `GPT2:GREEDY`, then feeds selected identifiers until exactly 64 continuation identifiers are staged. It closes the model before decoding once through `BPR-DECODE`, and writes only the decoded bytes to stdout. Any failure emits no text, closes a live model exactly once, and exits nonzero. `GPT2-REFERENCE` remains test-only comparison data; add the exact decoded-byte reference needed by the proof.

Owner: direct single-process GPT-2 prompt-to-text command only. The existing singleton BPE implementation stays required in place; this command creates no tokenizer owner or second BPE algorithm. Acceptance: the exact `Hello` command on DGX Spark matches all 64 `GPT2-REFERENCE` identifiers and the exact decoded bytes; the merges digest is enforced; the staged identifier bound has a canary; empty or over-capacity prompt and model-open failure publish no stdout; and `SAFET`/mapping owners return to their starting counts. The test exercises the same CLI generation path, not a copied loop.

Forbidden: `INFER`, engine, sequence, scheduler, paging, cache abstraction, runtime, registry, server, HTTP, sampling configuration, random state, tokenizer refactor, tokenizer type, `MODEL-ASSET`, BPE relocation, second forward loop, allocation layer, ABI/version, compatibility path, manifest, lint, suite enrollment, skip, or performance assertion. Smallest owning check: the exact real-device CLI proof with `<model-root>` and prompt `Hello`.

Claim: agent=codex-gpt2-cli workspace=.jj-ws/habu-infer-engine-64-02416606.
