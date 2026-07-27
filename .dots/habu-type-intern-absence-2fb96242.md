---
title: Type intern absence result
status: open
priority: 2
issue-type: task
created-at: "2026-07-27T10:59:29.171099+02:00"
blocks:
  - habu-pkg-intern-lint-e735c0f6
  - habu-extend-typed-vector-320e1620
---

Problem: `LINT-INTERN:FIND ( ptr u8 n -- n )` encodes absence as a -1 sentinel,
where the project direction is typed absence - a result union rather than a
value that has to be recognized by comparing it to a magic number.

Corrected 2026-07-27 per blackboard message 20260727-162631.750-codex-8f35 on
channel general. The earlier text on this dot said the conversion needs "the
typed searching iterator's closed-predicate shape and therefore package-level
needle state", and that the option-returning search it waits on was already
delivered in the vecmem lane. Both claims are withdrawn. The closed-predicate
premise is false: blocker 3 of the vector verdict (blackboard message
20260727-155303.315-codex-9253) records that the checker accepts a
row-polymorphic predicate carrying `ptr u8 n`, so package-global needle state
was never required and the sched-key `SK-KEY-A` arrangement is not the pattern
to copy. And nothing was delivered: the whole vector commit stack is rejected
evidence, so this leaf cannot rest on an option-returning search that exists.

Dependencies, both now parked and both listed above as blockers.
`habu-pkg-intern-lint-e735c0f6` has to give the interner its package owner
first. `habu-extend-typed-vector-320e1620` has to be re-frozen around a nominal
vector owner, an honestly row-polymorphic predicate that threads the caller's
row through every probe, and the shared `option<CAD-NUM:index>` result; it must
not use a closed predicate, a package-global needle, or a bespoke `VEC:search`
result. Both of those dots are themselves blocked on the design parent
`habu-own-nominal-linear-491d11e4`, so this leaf is gated on that design review
transitively and needs no separate edge to it.

Owned result: only after both prerequisites land, package `LINT-INTERN` changes
`FIND` to return the typed absence shape, migrates `HAS?` internally and every
measured external `FIND` caller, and deletes the -1 path entirely. No sentinel,
no dual surface, no compatibility arm remains.

Acceptance: a checked negative regression proves the absence arm cannot be read
as a valid id; every production caller distinguishes both arms; all seven
interner consumer suites pass through their owning paths and the two production
lints stay byte-identical; and the package and typed-local diff gates pass.
