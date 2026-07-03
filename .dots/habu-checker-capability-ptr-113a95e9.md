---
title: "Checker capability: ptr arithmetic"
status: open
priority: 2
issue-type: task
created-at: "2026-07-01T23:07:20.859895+02:00"
---

Pointee-polymorphic pointer arithmetic: prim rows for ptr T + n -> ptr T (and -, 1+, cell+) plus an explicit ptr a -> ptr u8 byte-view word. Retires ~38 TRUSTED: sites: every *-BYTE+ helper (struct/M-/SHK-/CODE-/CRH-/BFR-), src/os env-base ARGV/GETENV/TMP-PATH cascade, script-argv x7, bundle-argv x4. Add negative regressions (ptr T + ptr T rejected; cross-pointee unify rejected). Effort S/M (~4d). Touches src/core/checker.f prim table + docs/effects.md.
