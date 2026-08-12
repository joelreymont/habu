---
title: Hoist loop-invariant values out of the body
status: active
priority: 2
issue-type: task
created-at: "\"2026-08-10T07:21:39.129719+02:00\""
---

COUNT-DOWN's emitted loop holds mov x2,#0 INSIDE the body and compares against it each turn (loops lane 2026-08-10, re-attribution: its 20-byte gap vs clang's csinc is mostly a missing compare-immediate fold - habu-compare-against-a-da4cc639 owns that - plus loop-invariant code motion). A value defined in the loop from loop-invariant operands wants defining once in the preheader. Home: derive whether combine.f's within-block shape can see it (it cannot move across blocks today) or whether the new HIR pass from habu-close-the-loops-1571fb6f is the seat - the loop structure is visible there. MANY-LOCALS' 7-add invariant chain is the same class (its closed-forming already hoists it; a general LICM covers non-closed-formable loops). Acceptance: COUNT-DOWN's residual gap after the cmp fold measured and reduced; no row regresses; direct-refinement evidence. Files: the closed-forming pass's home. Depends: habu-close-the-loops-1571fb6f (shares the pass).

PRESSURE-LOOP IS NOW THIS DOT'S ROW (midblock probe 2026-08-12, full
record on 4145325c's archived leaf): the middle-block spill is refuted
(zero real-tree consumers), and the hoist closes the judge's LAST refused
row. The probed design: the pass seat is src/compiler/native/loop.f (the
closed-forming pass - already recognises ?do with the pre-header, already
has FUN-DEF, runs in migrate.f CLOSED on the HIR before selection); the
soundness precondition is ONE SCAN of the schema's own declared effects -
no operation in the loop declares a WRITE, no call, no trap (hir.f
IR--SCHEMA-EFFECT read via EFFECT@/FEFFECT@) - not an alias analysis.
After hoisting loads AND their invariant adds, the body is one addition
into one accumulator - loop.f's recognised closed-form shape, so the row
closes the way clang's twin does (never holds 14 values; clang: 72B,
0.06ns vs engine 1096B, 199.61ns). MEASURED WALLS for the fixture pair:
13/14 in-body; hoisted-loads-alone makes it WORSE (block-argument classes,
neither spillable nor rematable) - the hoist must take the invariant adds
too; entry-block values spill fine (18->1 slot). FIRST HAZARD, named:
hoisting every memory op leaves the loop's token block arguments unread -
the pass relies on select.f DDROP? (source-module half owned by
habu-drop-a-dead-1c0ff5a2); a Road-2 worker hits this first. FALLBACK if
a write really aliases: re-association (exact for wrapping add; no width
wall until E-NTAPE-CAP at 28). METADATA: this leaf and 7bcfd6c5 cite
habu-close-the-loops-1571fb6f as a dependency - it LANDED as loop.f and
is in no .dots directory; the dependency is satisfied, not blocked.

Claim: agent=hoist workspace=.jj-ws/habu-hoist
