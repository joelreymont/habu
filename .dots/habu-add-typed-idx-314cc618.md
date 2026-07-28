---
title: Add typed index byte arithmetic
status: active
priority: 1
issue-type: task
created-at: "2026-07-28T01:34:31.462188+02:00"
---

Problem: the checked GPT-2 F32 reader cannot compare a nominal index with an
item count or turn an index and element width into a byte offset without
erasing their roles.

Owner and interface: package `CAD-NUM` in `lib/cad-num-arithmetic.f` owns:

- `INDEX-IN-COUNT? ( index item-count -- bool )`;
- `INDEX-BYTE-OFF ( index byte-len -- numeric-result<byte-off> )`;
- `BYTE-OFF-IN-LEN? ( byte-off byte-len -- bool )`.

Both comparisons are strict. The product returns `ok` only when representable
and `overflow` otherwise. Reuse only CAD-NUM's private projections, existing
numeric-result constructors, `DIV-BYTES-FLOOR`, and `ADVANCE-BYTE-OFF`. Add no
TRUSTED word, nominal role, public raw projection, generic `n` arithmetic,
pointer, MEM, or GPT-2 code.

Tests cover zero, one, equality, maximum values, maximum-safe and first-
overflow products in both operand orientations, far wrap including maximum
times maximum, exact result arms and values, and nominal role swaps. Full-cell
properties use an independently commuted quotient oracle, prove every generated
bit takes both values, and exercise lower, upper, and cross-band pairs. Reuse
the existing exhaustive `BO-CODE` result classifier; do not duplicate its
MATCH. Mutations accepting equality, masking an operand, checking a wrapped
product or its sign, swapping roles, dropping any generator limb, returning the
wrong arm, or exposing a raw projection must fail. Register the focused suite
in the standard-library gate and its derived manual schedule.

Design authority: amend `MODEL-CAD-V2-PLAN.md` B5.2 with the three exact rows
and boundary classes. Admit only index times element-byte-width from the prior
blanket multiplication ban. State that both predicates return booleans, not
persistent bounded-index evidence.

Authority correction: the exact tree already loads this arithmetic from
`lib/memory.f` and inference modules, so inherited claims that CAD-NUM has no
production loader or that its type test is its sole consumer are false. Correct
those statements in `lib/cad-num-types.f`, `lib/cad-num-types-test.f`,
`FILEMAP.md`, and the two module rows in `lib/std.manifest`; make no other
change to the type slice.

Files: `lib/cad-num-arithmetic.f`, `lib/cad-num-arithmetic-test.f`,
`lib/cad-num-types.f`, `lib/cad-num-types-test.f`, `lib/std.manifest`,
`FILEMAP.md`, `test/gate-stdlib-cases.f`,
`tools/suite-coverage-lint-core.f`, and `MODEL-CAD-V2-PLAN.md` only.

Acceptance: focused arithmetic suite, exact owning load, typed-local, package,
manifest, file-map, trust, suite-coverage, owning standard-library gates, and
an exact source/plan/manifest interface census pass. The standard-library gate
must execute the focused suite, whose checked F32 composition certifies without
a cast. The final tree must contain no false no-production-loader or sole-
consumer claim for either CAD-NUM slice.

Claim: agent=cad-index-bytes workspace=.jj-ws/habu-add-typed-idx-314cc618
