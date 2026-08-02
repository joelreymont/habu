---
title: Resolve native full namespace paths
status: active
priority: 1
issue-type: task
created-at: "2026-07-31T06:34:59.946328+02:00"
---

Source dependencies: exact reviewed E1 namespace rows and the HB package hard
cut. Owner: `ENGINE-EMIT`.

Emit one private `LQCLASS` classifier shared by `LFIND`, qualified definition,
and `EXPORT`. Input x9/x10 is the full token. Output x17 is x10 for an
unqualified token, the last-colon index for a valid qualified token, or -1 for
leading, trailing, or doubled separators. Preserve x9/x10 and use no second
scanner. Reopen `ENGINE-EMIT` around `EMIT-FIND` so the classifier and E1
`LNSFIND` remain private without forwarding words.

`LFIND` saves full length and split across the frozen `LNSFIND` clobber set,
resolves the full prefix, and accepts package or type rows. One emitted
full-prefix ensure walker reuses `LNSFIND` and package-row creation for every
prefix. Qualified definition creates missing package prefixes, accepts an
existing package row, and rejects an existing type row. `EXPORT` publishes
only the tail after the last colon, so `A:B:C:WORD` creates alias `WORD` and
resolves through the production lookup path. Preserve E1 row shape and
rollback.

Write set: `src/habu/habu1.f`, `src/habu/habu2.f`,
`test/gate-dictionary-lib.f`, and `test/type-export-suite.f`. Any surviving
source `TRUST` keeps only its source-local rationale, retirement owner, and
focused production test. Do not add a parent link, side table, schema, version,
compatibility spelling, second scanner, ancestor lookup, using change, nested
package blocks, recovery edit, or lint.

Acceptance: deep lookup, definition, and `EXPORT` work at every supported
depth; malformed paths publish nothing; package and type lookup work; defining
into a type rejects; shallow behavior remains exact. Run the focused dictionary
and export suites, package and typed-local gates, native fixpoint, and full
native gate.

Claim: agent=e2a_native_impl workspace=.jj-ws/habu-resolve-native-full-52296eac.
