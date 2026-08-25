---
title: "BPE: factor full-vocab parser"
status: open
priority: 1
issue-type: task
created-at: "2026-07-23T09:46:59.317971+02:00"
---

Problem: the checked full `vocab.bpe` parser, token-string map, and resolved 50,000-row tables are embedded in the runtime installer `maki/examples/nanogpt/bpe-full.f`. The compact-table generator needs the same authoritative parse result; copying that parser would create two semantic authorities.

Required result: move only the parser storage and helpers into
`maki/examples/nanogpt/bpe-vocab-parse.f` under package `BPE-FULL`. Define
private `PARSE-FILE ( ptr u8 n -- n )`: it reads one path, rejects absence,
malformed header/rows, forward references, duplicate token strings, count or
arena overflow, and returns the exact resolved merge count while leaving the
resolved child arrays and token-string map visible only to later files that
reopen `BPE-FULL`. It must not install or mutate the live BPE vocabulary.
Migrate the runtime loader to the same package with public `PRESENT?` and
`LOAD`; `LOAD` calls `PARSE-FILE` and performs the existing single validated
`MAKI:BPR-INSTALL`. Move its three errors to short qualified names in the same
public package. Delete `MAKI:BPF-PRESENT?`, `MAKI:BPF-LOAD`, global
`E-BPF-*`, and every alias; migrate the finite caller set atomically.

Prerequisites: the landed real-vocabulary loader. The package migration's
finite caller set must migrate ATOMICALLY - every caller moves or requalifies
in the same change, no migration exception, no legacy caller left behind. Owned result: one shared parser implementation, its
private resolved tables, and the narrow `BPE-FULL` loader boundary only. It
does not own encoder.json validation, compact selection, rendering, or
tokenizer state.

Acceptance: the production full loader remains byte-for-byte equivalent in
loaded merge structure and identifiers; a previously installed vocabulary
survives every malformed parser fixture unchanged; missing header, missing
separator, duplicate token, forward reference, extra/missing row, arena/table
overflow, and trailing malformed data reject by the qualified named errors; a
structural test proves parser logic exists once, the public inventory is
exactly the errors plus `PRESENT?` and `LOAD`, and every parser definition is
private. Files: `bpe-vocab-parse.f`, reduced `bpe-full.f`, all direct callers,
focused parser/full-loader tests and manifests. Smallest owning-path
check: `BPE-FULL:LOAD` loads a compact valid vocab fixture, compares every
resolved row, then injects one malformed row and proves the prior vocabulary
is unchanged. Also run exact typed-local, package, and host checks.
Claim: unassigned.
