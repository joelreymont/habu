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

Acceptance: move pre-splitting into package `BPE-SPLIT`. Its public `CHUNK-LEN`
word has effect `( ptr u8 n n -- n )`: counted input, byte cursor, and byte
length of the next GPT-2 chunk. It consumes `UTF8:NEXT` and
`UNICODE-CLASS:LETTER?`, `NUMBER?`, and `WHITE-SPACE?`; it owns no mutable
cursor, range table, return buffer, or scratch state. The cursor must be in the
input span, the returned length must be positive and remain within that span,
and malformed UTF-8 must use `UTF8:NEXT`'s raw-lead result: one exact lead byte,
one byte of progress, and no overread. Remove `BPE-CP@`, the bounded Unicode
tables, their lookup state, and `BPE-CHUNK-LEN`; migrate both BPE encoding and
training to `BPE-SPLIT:CHUNK-LEN` without a compatibility alias.

Pin exact chunk boundaries and sequential real-token identifiers against the
reference for ASCII adjacency, Thai,
Devanagari, Greek, Cyrillic, Hebrew, Arabic, CJK, Hiragana, Katakana, fullwidth
letters/digits, non-BMP letters/numbers, every White_Space range, combining
marks, punctuation, malformed byte sequences, and multilingual mixtures. Run a
deterministic corpus residual scan and prove zero unexplained divergence.
Repeated calls plus nested and interleaved split scans over two explicit input
cursors must be byte-identical. Whole-tokenizer instance ownership and
interleaved encoding belong to `habu-own-nanogpt-tokenizer-211fd3ac`. The old
bounded-block fixture flips to the exact reference result. Timing is excluded;
measure tokenizer throughput later in the sole evidence lane.

Files: BPE pre-split integration package, real-vocabulary fixtures, focused
chunk/token parity tests, manifests, and `FILEMAP.md`. Verify BPE/tokenizer/full
vocabulary tests, reference chunks/identifiers, malformed/nested/interleaved
cases, package/typed-local/host/filemap/dot lints, and owning Maki gates.

Dependencies: `habu-bpe-real-vocab-c973932a`,
`habu-bpe-unicode-data-45a7c2e9`, and `habu-bpe-utf8-scalar-8c1d6f34`.
Ownership: Unicode-aware GPT-2 pre-split integration and parity only. It does
not own BPE vocabulary storage, encode workspace, or tokenizer instances.
