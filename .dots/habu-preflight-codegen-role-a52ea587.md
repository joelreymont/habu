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

Resolution: route (b), structural assertion over real emitter output.
tools/codegen-role.f extracts the guarded definitions/use sites from the actual
stage sources, CHECK!-certifies them, compiles and runs them on the live arm64
emitter primitives (asm.f/icode.f/mnem.f), and decodes the emitted words plus
fixup records: every C-LOCAL-REF CLOC-MAIN use must record one pending
label-relative B26 fixup targeting the CLOC-MAIN label (word $14000000), and
each spawn zero loop must emit movz xN,#0 followed by exactly size/8 stores of
that register to [SP, base..base+size-8]. tools/codegen-role-test.f (gate suite
codegen-role; also forked in stdlib/tail-pure) covers the real sources plus
four corruption fixtures - both historic must-lack forms (checker-verdict
E-CGR-EVAL) and two compiling same-type corruptions (`14 14` base register ->
E-CGR-SPAWN; `BL,` call -> E-CGR-CLOC). All four textual asserts and
BF-PREFLIGHT-HABU1/HABU2 retired from tools/build-fixpoint.f; BF-PREFLIGHT-ICODE
kept (separate blocker above). Engine bugs found and dotted along the way:
habu-undefined-word-in-d9dc3452 (undefined in evaluate under catch crashes
natively), habu-check-records-go-4f62cd2e (CHECK! records stale across ndict
rollback).
