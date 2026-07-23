---
title: "BPE: install Unicode chunk matcher"
status: open
priority: 1
issue-type: task
created-at: "2026-07-23T09:47:20.909072+02:00"
blocks:
  - habu-bpe-unicode-data-45a7c2e9
  - habu-bpe-utf8-scalar-8c1d6f34
---

Problem: production BPE encoding and training still use the legacy bounded Unicode matcher and ambient helper state. Complete Unicode tables and strict scalar decoding are already landed, but no stateless GPT-2 grammar owner composes them.

Required result: create package `BPE-SPLIT` with one public word `CHUNK-LEN ( ptr u8 n n -- n )`. It receives a counted byte span and byte cursor, returns the positive byte length of the next GPT-2 regex chunk, and implements alternatives in exact leftmost greedy order: contractions, optional-space Letter run, optional-space Number run, optional-space nonspace/non-Letter/non-Number run, end-of-input whitespace run with one whole trailing scalar retained when followed by nonspace, then ordinary whitespace run. It consumes `UTF8:NEXT` plus complete `UNICODE-CLASS:LETTER?`, `NUMBER?`, and `WHITE-SPACE?`. Malformed UTF-8 uses the decoder raw-lead result: one exact source byte, one byte of progress, and no overread. Negative length, cursor before zero, cursor at/after end, or any computed span escape throws the existing string-bounds error. The package owns no mutable cursor, range table, return buffer, or scratch state.

Migrate both production `BPE-ENCODE` and `BPE-TRAIN` chunk traversal to `BPE-SPLIT:CHUNK-LEN`. Delete `BPE-CP@`, bounded Unicode tables and lookup state, `BPE-CHUNK-LEN`, and every compatibility alias. Preserve arbitrary-byte round trips.

Prerequisites: `habu-bpe-unicode-data-45a7c2e9` and `habu-bpe-utf8-scalar-8c1d6f34`. Owned result: chunk grammar and production routing only. It does not own reference corpus data, compact vocabulary generation, encode workspace capacity, or tokenizer instances.

Acceptance: focused production tests pin ASCII adjacency and contractions, every White_Space form, Letter/Number adjacency across representative scripts and non-BMP scalars, combining marks, punctuation, malformed/truncated/overlong bytes, multiple whitespace scalars before nonspace and at end of input, bounds, repeated calls, nested scans, and interleaved scans over two explicit cursors. Mutating alternative order, whitespace backtracking, raw-lead progress, or either production caller makes a focused test fail. Public inventory is exactly `CHUNK-LEN`; all legacy names reject. Files: `bpe-split.f`, its focused test, `bpe.f`, manifests, `FILEMAP.md`. Smallest owning-path check: encode and train one fixture whose correct result changes when either caller uses the legacy matcher, plus the focused grammar suite. Also run exact typed-local, package, host, and file-map checks. Claim: unassigned.
