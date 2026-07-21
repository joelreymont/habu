---
title: "BPE: integrate complete Unicode split"
status: open
priority: 1
issue-type: task
created-at: "2026-07-21T19:22:10+02:00"
blocks:
  - habu-bpe-real-vocab-c973932a
  - habu-bpe-unicode-data-45a7c2e9
  - habu-bpe-utf8-scalar-8c1d6f34
---

Problem: the BPE matcher must compose strict scalar decoding with complete
Unicode Letter/Number/White_Space classification and the GPT-2 regex chunk
grammar. Unit proofs of the tables and decoder do not prove chunk or token
parity.

Acceptance: move BPE pre-split mechanics into a package-owned consumer of the
two prerequisite APIs, with explicit scanner state and no shared global cursor.
Preserve the pinned policy that malformed input returns one raw lead byte and
consumes and advances exactly one byte without overreading. Pin exact chunk boundaries and
real token identifiers against the reference for ASCII adjacency, Thai,
Devanagari, Greek, Cyrillic, Hebrew, Arabic, CJK, Hiragana, Katakana, fullwidth
letters/digits, non-BMP letters/numbers, every White_Space range, combining
marks, punctuation, malformed byte sequences, and multilingual mixtures. Run a
deterministic corpus residual scan and prove zero unexplained divergence.
Repeated, nested, and interleaved tokenizations are byte-identical. The old
bounded-block fixture flips to the exact reference result. Timing is excluded;
measure tokenizer throughput later in the sole evidence lane.

Files: BPE pre-split integration package, real-vocabulary fixtures, focused
chunk/token parity tests, manifests, and `FILEMAP.md`. Verify BPE/tokenizer/full
vocabulary tests, reference chunks/identifiers, malformed/nested/interleaved
cases, package/typed-local/host/filemap/dot lints, and owning Maki gates.

Dependencies: `habu-bpe-real-vocab-c973932a`,
`habu-bpe-unicode-data-45a7c2e9`, and `habu-bpe-utf8-scalar-8c1d6f34`.
Ownership: Unicode-aware GPT-2 pre-split integration and parity only.
