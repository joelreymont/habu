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

LINT-INTERN:FIND ( ptr u8 n -- n ) signals absence with a -1 sentinel - value-encoded absence where the project direction is typed absence (result unions over sentinels). It was deliberately left out of the interner packaging work, which is owned by habu-pkg-intern-lint-e735c0f6, because the conversion is a public-interface change: it needs the typed searching iterator's closed-predicate shape and therefore package-level needle state (the maki/sched-key SK-KEY-A arrangement). The typed option-returning VEC:FIND it waits on is owned by habu-extend-typed-vector-320e1620, codex frozen shape ( R ptr a [ R CAD-NUM:index -- R bool ] -- R option<CAD-NUM:index> ) reusing lib/adt/option.f. Both are recorded above as blockers, replacing the raw commit references this dot used to carry. Both are delivered in the vecmem lane only and are not on master yet, so this leaf cannot start until they are integrated.

Owned result: after those two land, convert LINT-INTERN:FIND to return the typed absence shape, migrate its callers (HAS? internally plus the measured external FIND callers), and delete the -1 sentinel path entirely - no dual surface, no compatibility arm. Owner: package LINT-INTERN. Acceptance: a checked negative regression proving a caller cannot read the absence arm as a valid id; all seven consumer suites green through their owning paths; both diff lints; the two production lints byte-identical.
