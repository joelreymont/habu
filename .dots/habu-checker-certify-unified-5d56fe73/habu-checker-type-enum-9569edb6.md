---
title: "Checker: type ENUM construct match"
status: active
priority: 1
issue-type: task
created-at: "\"2026-07-13T17:14:49.624384+02:00\""
blocks:
  - habu-checker-type-structure-d996215b
---

Own construct/MATCH effects and field-aware diagnostics for unified ENUM. Route
every payload consumer through one accessor over declared variant fields,
including constructor effects, MATCH arm effects, width/padding, and
`TFAM-CONCRETE-LINEAR?`. Instantiate named payload fields in declaration order,
preserve generic substitution and transitive linearity, require exhaustive
variants, and report variant plus field name on mismatch.

Checker-miss RCA evidence: the static invariant is that a concrete ENUM holding
a linear named payload cannot be duplicated or discarded. The production
standard-input path was fail-closed. From the repository root, this exact input
command exited 70:

```sh
printf '%s\n' 'require test/checker-assert.f' 'deflinear mtok' \
  ': ECL-CONTROL ( mtok -- mtok mtok ) dup ;' |
  HB_TMP=/tmp/habu-enum-rca-control bin/hb
```

Standard output was empty. Standard error was exactly:

```text
habu: in ecl-control: at 'dup'
hook: non-certified definition: ecl-control at 'dup'
```

On that same `bin/hb` standard-input path, the following minimal named ENUM and
both invalid definitions were accepted with exit 0 and empty standard output
and standard error; `CHECK-QUIET-CANDIDATE!` also accepted both definitions:

```forth
deflinear mtok
ENUM-DECL:ED-RUN ecl 0
   VARIANT hold FIELD token mtok ;VARIANT
;ENUM
: ECL-DUP ( ecl -- ecl ecl ) dup ;
: ECL-DROP ( ecl -- ) drop ;
```

This is a checker-semantics miss in a legacy payload consumer, not a runtime or
library bug: `TFAM-CONCRETE-LINEAR?` walks only `SUMV-SCH-COUNT@` and
`SUMV-SCH-START@`, so it never sees the named field stored in the shared field
registry. The durable repair is exact: in the sum/ENUM branch, iterate from zero
to `vid SUMV-PAY-N`, resolve each element with
`vid j SUMV-PAY-ROOT SCHEMA-ROOT@`, and pass that node to `TFCL-NODE-XT`.
This makes concrete-linearity use the same unified declared-payload accessor as
construct, MATCH, width, and padding. Re-run the exact fail-closed control and
accepted-bad probes before the change, then re-run them green after the change.
Add negative regressions in which both minimal definitions reject specifically
for linearity, not for an unrelated syntax or unknown-type reason. A runtime
guard, library check, hidden unchecked boundary, or test-only rejection is
forbidden; the checker must reject the program before runtime. Add positive compact,
payload, nested, generic, exhaustive-MATCH, field-diagnostic, rollback, and
snapshot cases. Remove every direct legacy `SUMV-SCH-COUNT@`/
`SUMV-SCH-START@` payload walk from semantic consumers. Verify the focused
checker/type-family/declaration suites and the owning native gates.

Claim: RELEASED 2026-07-21. The `enumcert` workspace is preserved as evidence,
but its implementation is obsolete and must never merge: destruction review
found that concrete-linearity still bypassed named payload fields.

Claim: agent=enumcert_impl workspace=.jj-ws/habu-checker-type-enum-9569edb6 machine=spark
