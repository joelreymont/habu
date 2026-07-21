---
title: "BPE: pinned Unicode class data"
status: closed
priority: 1
issue-type: task
created-at: "2026-07-21T19:22:10+02:00"
closed-at: "2026-07-21T22:44:11.502394+02:00"
close-reason: Pinned Unicode 16 data, checked generation, independent verification, and complete scalar classification passed the full native gate on b925e8e427ab.
---

Problem: the current Letter/Number table is bounded by blocks exercised by one
vocabulary, and the table-generation recipe relies on uncommitted Python. That
cannot prove the GPT-2 regex classes for arbitrary Unicode bytes.

Acceptance: pin one Unicode Character Database version and the matching
reference tokenizer/regex versions with authoritative input digests. A checked
Habu generator parses the pinned General_Category and `White_Space` inputs,
canonicalizes complete scalar ranges for every `L*`, `N*`, and White_Space
member, and emits package-owned immutable runtime tables plus version/input/
output hashes. A checked verifier rejects missing, overlapping, unsorted,
noncanonical, surrogate, out-of-range, or digest-mismatched rows and proves a
fresh regeneration byte-identical. Exhaustively compare all Unicode scalar
values with the pinned reference classification, including every range endpoint
and gap. No Python, shell, awk, sed, hand-edited range, or current-vocabulary
domain restriction remains.

Files: one Unicode data package, one checked generator/verifier tool package,
pinned input/provenance documentation, focused tests, manifests, and
`FILEMAP.md`. Verify regeneration/digests, exhaustive classification, malformed
input/capacity/rollback tests, table byte measurement, package/typed-local/host/
filemap/dot lints, and the owning native tool/test gates.

Dependencies: none. Ownership: pinned Unicode inputs, canonical L/N/White_Space
range generation, runtime tables, and exhaustive class proof only. UTF-8 byte
decoding and BPE chunking remain separate dots.

Claim: agent=unicode_data_impl workspace=.jj-ws/habu-bpe-unicode-data-45a7c2e9 machine=spark
