---
title: "Checker capability: ptr arithmetic"
status: open
priority: 2
issue-type: task
created-at: "2026-07-01T23:07:20.859895+02:00"
---

Pointee-polymorphic pointer arithmetic: prim rows for ptr T + n -> ptr T (and -, 1+, cell+) plus an explicit ptr a -> ptr u8 byte-view word. Retires ~38 TRUSTED: sites: every *-BYTE+ helper (struct/M-/SHK-/CODE-/CRH-/BFR-), src/os env-base ARGV/GETENV/TMP-PATH cascade, script-argv x7, bundle-argv x4. Add negative regressions (ptr T + ptr T rejected; cross-pointee unify rejected). Effort S/M (~4d). Touches src/core/checker.f prim table + docs/effects.md.

## Audit refresh (2026-07-06, head 1eb3b5d3)

The prim-table half is landed: `ptr a + n -> ptr a` / `n + ptr a` (src/core/
checker.f:3711-3712), `-` (:3714-3715), `1+`/`1-` (:3723-3726), with this dot's
negative regressions in test/engine-suite.f:380-385 (CBAD-PTR-ADD-PP,
CBAD-PTR-WIDEN-CELL/U32/NEST, CBAD-PTR-UNIFY-EQ/-REV all T-CHECK-REJECTS).
Remaining scope: the explicit `ptr a -> ptr u8` byte-view mint word and the
consumer migration of the ~38 TRUSTED: sites (env-base cascade, script-argv x7,
bundle-argv x4, *-BYTE+ helpers) onto the new rows.
