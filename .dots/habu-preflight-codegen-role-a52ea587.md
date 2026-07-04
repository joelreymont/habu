---
title: Preflight codegen-role check
status: open
priority: 2
issue-type: task
created-at: "2026-07-04T11:00:00.000000+02:00"
---

The BF-PREFLIGHT asserts kept in tools/build-fixpoint.f after the certify
retirement guard same-type codegen roles the checker cannot express, so they
survive the blocking native HOOK check that now covers the rest:

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

Separately, the BF-PREFLIGHT-ICODE asserts stay gated on the BF-CERTIFY blocking
flip (icode.f is emitted in the check-off window, so only the non-blocking
static certification covers it) - tracked by habu-checker-self-typing, not here.
