---
title: Preflight codegen-role check
status: open
priority: 2
issue-type: task
created-at: "2026-07-04T11:00:00.000000+02:00"
---

The BF-PREFLIGHT asserts kept in tools/build-fixpoint.f after the certify
retirement guard same-type codegen roles the checker cannot express, so they
survive the blocking native HOOK check and the blocking static certify that
now cover the rest:

- habu2: `CLOC-MAIN LABEL@ B,` must-have vs `CLOC-MAIN @ B ;` must-lack
  (label-relative branch vs raw fetch-branch + early word end).
- habu1: `14 SP SPAWN-ADESC-OFF SZA-I @ + STR,` must-have vs
  `14 SP SPAWN-ADESC-OFF + over + STR,` must-lack (the exact spawn
  descriptor-slot address computation - the historic Darwin spawn-underflow
  guard). Both forms are stack-neutral / same-type, so the checker accepts both.

Build a real codegen/role check so these last textual asserts can retire: e.g. a
typed role distinguishing `LABEL@` from `@`, or a structural assertion over the
emitter output/IR for the descriptor-slot address. Until then the two asserts
stay in BF-PREFLIGHT-HABU2 / BF-PREFLIGHT-HABU1 and reference this dot.

icode's typed-shape asserts (originally kept while BF-CERTIFY was non-blocking)
retired when the blocking flip landed; its remaining asserts are runtime
invariants (mmap fail-closed, no-static-allot executable memory), kept at the
site with their rationale - out of this dot's scope.

Ported from the fable lane (c33ec3e66479) onto the maki-type-families line.
