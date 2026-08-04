---
title: "BPE: prove Unicode tokenizer parity"
status: open
priority: 1
issue-type: task
created-at: "2026-07-21T19:22:10+02:00"
blocks:
  - habu-bpe-own-parity-00afdbd5
  - habu-bpe-factor-full-62bbd484
  - habu-bpe-install-unicode-3c84e7a1
  - habu-bpe-remove-chunk-acc70f20
  - habu-bpe-verify-gpt-543d4c98
  - habu-bpe-generate-compact-2d82e099
---

Problem: the tokenizer needs one production-path proof that the complete
Unicode chunk grammar, authenticated compact vocabulary, and legal encode
capacity compose to the exact GPT-2 result. Unit tests for those owners cannot
prove end-to-end chunk and token parity.

Required result: migrate all real-vocabulary tests and consumers to
`BPE-PARITY`, the generated `BPR-D-LOAD`, and
`BPE-SPLIT:CHUNK-LEN`. Scan every parity fixture through the real production
entry points. For every `BPE-PARITY:UNICODE` row, compare the complete sequence
of returned chunk lengths and real token identifiers. For every
`BPE-PARITY:FIXTURE` row, compare the complete production token sequence.
Process exactly the published row counts, report the number of mismatched
fields, and require zero; an omitted, duplicated, reordered, wrong-boundary, or
wrong-identifier row must change the result. Run the same Unicode identifier
rows after the authenticated full 50,000-merge load and prove they remain
exact. Flip the old bounded-block divergence to its pinned reference result and
delete every obsolete fixture, compact-table, or legacy matcher reference.

The compact test is hermetic. The full-artifact leg is mandatory for this
landing on the pinned real files and may remain an explicitly reported optional
developer leg afterward because the large regenerable files are not committed.
Timing is excluded; it belongs to the sole tokenizer evidence lane.

Prerequisites: all six child dots in this subtree. Owned result: final consumer
wiring, deterministic corpus residual accounting, full-table confirmation, and
suite registration only. It does not own corpus data, parsing, generation,
chunk grammar, encode workspace, tokenizer instances, or throughput policy.

Acceptance: exact chunk and token parity covers ASCII adjacency, Thai,
Devanagari, Greek, Cyrillic, Hebrew, Arabic, CJK, Hiragana, Katakana, fullwidth
letters/digits, non-BMP letters/numbers, every White_Space range, combining
marks, punctuation, malformed bytes, and multilingual mixtures. Mutations to
one chunk, token id, descriptor count, row order, production caller, compact
merge, or full-table load fail. All old public fixture and matcher names reject.
Files: focused split/parity/full-vocabulary tests, suite manifests, requires. Smallest owning-path check: one production corpus scan returns
zero, then a one-token mutation returns exactly one. Verify focused BPE,
tokenizer, full-vocabulary, and owning Maki tests plus exact typed-local,
package, host, and dot checks.

Claim: unassigned. Rejected evidence remains at `7ba07d495513`,
`ee013a7e`, and `6d62573c78e0`; do not recover those revisions wholesale.
