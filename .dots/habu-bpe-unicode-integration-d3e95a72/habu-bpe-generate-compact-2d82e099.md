---
title: "BPE: generate compact GPT-2 tables"
status: open
priority: 1
issue-type: task
created-at: "2026-07-23T09:48:56.762562+02:00"
blocks:
  - habu-bpe-verify-gpt-543d4c98
---

Problem: `BPR-D-BYTEID`, `BPR-D-MA`, `BPR-D-MB`, `BPR-D-MID`, and `BPR-D-MERGES` are hand-edited under a false generated-data header. The missing generator makes compact-rank priority, child remapping, and source provenance unrepeatable.

Required result: implement checked `tools/bpe/compact-generate.f` using the authenticated private tables from `habu-bpe-verify-gpt-543d4c98` and the immutable `BPE-PARITY` corpus. For every expected fixture token id in 256..50255, recursively mark its original merge rank and every merged child in the production parsed merge DAG. Reject the special id or any unknown id. Scan original ranks in ascending order, emit every marked rank exactly once, and map a byte child to its byte id while mapping an earlier merged child to `256 + compact-index`. Emit real merged id `256 + original-rank`. This preserves original priority and produces the transitive closure needed to build every expected fixture token without simulating or copying the production BPE algorithm.

Render `maki/examples/nanogpt/bpe-real-data.f` deterministically as generated vocabulary data only: authenticated artifact sizes/hashes and generator identity in its header; private byte, child-a, child-b, and real-id arrays inside package `MAKI`; one public `BPR-D-LOAD`; no fixture strings, chunk rows, hand-owned tables, Python reference, network behavior, or large source artifact. The CLI takes explicit encoder path, vocab path, and output path, verifies both artifacts first, and writes atomically only after the complete render succeeds. Re-running on the same inputs is byte-identical.

Prerequisite: `habu-bpe-verify-gpt-543d4c98`, which already depends on the factored parser and parity corpus. Owned result: compact-closure selection, compact child remapping, deterministic rendering, and generated vocabulary module only. It does not own corpus evidence, tokenization, runtime vocabulary instances, or source fetching.

Acceptance: regenerate the committed module from the pinned real artifacts and byte-compare it; every compact child references a byte or earlier compact row; original ranks and real ids are strictly increasing and unique; deleting any required ancestor, changing one fixture id, reversing two ranks, mis-remapping one child, changing an artifact hash, or truncating output fails generation or the production parity suite. The generated module exposes only `BPR-D-LOAD`; its arrays are unreachable outside reopened `MAKI`. Source/DATA size remains within the measured small-fixture budget and no encoder.json or vocab.bpe payload is committed. Files: generator, focused synthetic generator test, generated `bpe-real-data.f`, manifests. Smallest owning-path check: regenerate from real artifacts, compare exact bytes, then load through public `BPR-D-LOAD` and encode every `BPE-PARITY:FIXTURE` through production. Also run exact typed-local, package, trust, and host checks. Claim: unassigned.
