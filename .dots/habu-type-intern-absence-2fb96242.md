---
title: Type intern absence result
status: open
priority: 2
issue-type: task
created-at: "2026-07-27T10:59:29.171099+02:00"
---

LINT-INTERN:FIND ( ptr u8 n -- n ) signals absence with a -1 sentinel - value-encoded absence where the project direction is typed absence (result unions over sentinels). Deliberately left out of the packaging commit 6c119985 because the conversion is a public-interface change: it needs the typed searching iterator's closed-predicate shape and therefore package-level needle state (the maki/sched-key SK-KEY-A arrangement). Owned result: after the typed option-returning VEC:FIND revision lands (codex frozen shape ( R ptr a [ R CAD-NUM:index -- R bool ] -- R option<CAD-NUM:index> ) reusing lib/adt/option.f), convert LINT-INTERN:FIND to return the typed absence shape, migrate its callers (HAS? internally plus the measured external FIND callers), and delete the -1 sentinel path entirely - no dual surface, no compatibility arm. Owner: package LINT-INTERN. Acceptance: a checked negative regression proving a caller cannot read the absence arm as a valid id; all seven consumer suites green through their owning paths; both diff lints; the two production lints byte-identical.
