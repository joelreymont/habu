---
title: "TFAM 12: layout-aware stack ops + width-aware lowering"
status: open
priority: 2
issue-type: task
created-at: "2026-07-03T23:36:48.939694+02:00"
---

PLAN.md item 12. Logical widths for all stack prims (dup/drop/swap/over/nip/rot/-rot/tuck/2dup/2drop/2swap/2over), JIT shuffles + spilled fallbacks, VOP*/VCMP/VUN/FOP one-cell lowering, return-stack transfers, locals, ?dup rejection, constant/depth/.s, interpret mode, nested evaluate/catch-throw/run-in-stack frame metadata; width facts reach native+Gforth emitters BEFORE emission; hidden fields cannot bind ordinary effects/quotations/combinators/control predicates; possibly-linear layout copies reject until TFAM 11. Gate 17h. Depends: TFAM 7.

Constant follow-up (from TFAM 5 const-b89c90f0): native C-CONSTANT + verify-source RECORD-DEFINER? + public-signatures PS-MAYBE-TRUST-DEFINER + all-errors CA-ADD-SUPPORT-CONSTANT all narrow a layout-family constant value to one-cell `-- a` (native accepts the constant, layout USE fails downstream). When this item makes `constant` reject (or multi-cell shape-carry) layout values at the value-pop, remove the `-- a` boundary comments at those sites and flip the parity fixtures `const-layout-narrow` (tools/check-all-errors-test.f) + PST-TEST-CONST-LAYOUT (tools/public-signatures-test.f) from "layout USE rejected" to accepted/shape-carried.


REMAINING after the 3b commit (checker flip + pass-2 width-aware lowering + snapshot format version): (1) constant shape-carry at the four value-pop sites (native C-CONSTANT habu2.f, verify-source RECORD-DEFINER?, PS-MAYBE-TRUST-DEFINER, CA-ADD-SUPPORT-CONSTANT) with the staged parity-fixture flips (CAE-TEST-CONST-CARRY, PST-TEST-CONST-CARRY, TD12-CONST) — separable value-pop change, own commit (3c); (2) interpret-mode wide values (top-level transports of layout values outside colon bodies) are REACHABLE TODAY via a TRUSTED boundary at the unchecked REPL and SILENTLY CORRUPT rather than failing closed: with `TRUSTED: MK ( -- pp<n,n> ) 7 9 ;`, top-level `MK dup . . .` prints `9 9 7` — the interpret-mode `dup` copies only the top (tag) cell of the 2-cell bundle instead of the whole group, and no reject/die fires (exit 0). This is NOT unreachable and is NOT waiting on items 8/9; it is its own capability gap, now tracked by dot habu-tfam-12-interpret-10b385b1 (interpret-mode width tagging or a top-level check hook so interpret-mode transports lower width-aware or fail closed); (3) does>-split bodies with wide facts currently FAIL CLOSED (named exit at the pass-2 trigger) — lift with a two-phase-aware token indexing or keep fail-closed with a checker-side reject; (4) the Gforth bootstrap mirror of the pass-2 lowering (bootstrap/cg/forth.fs + jit.fs) — no current gate exercises the bootstrap emitter (verified), tools/bootstrap.sh recovery is the exposure; (5) depth/.s logical-shape introspection (currently fail-closed reject over hidden-bearing rows); (6) committed snapshot doctored-trailer regression fixture needs a snapshot-test home (version bump itself proven by patched-image runs).

## Audit vs tip db88a576 (2026-07-09)

Baseline: the copied `bin/hb` was STALE vs tip `src/habu` (the interpret
engine-half + seal-watermark commits were not baked in) -> `test/seal.f` and
`test/gate-runner-entry-test.f` red (expected 83/64 got 0/77). A fixpoint
`install --force` refresh made both green; tip source is green modulo the
date-only `stale-status-lint` (STATUS.md Last-verified 07-08 vs today 07-09).
No tip code defect.

LANDED:
- Sub-dot 12-interpret ENGINE half — DNAME-WIDE (habu1/habu2/xref/layout/stage2/
  maker/bootstrap mirror), `wide-mark`/BWIDEMARK, XREF-WIDE?, LFIND bit,
  EM-INTERPRET-FIND + C-TICK LWIDE/LDIAGRET gate, diagnostic
  `interpret-mode layout value`, S2/MK source-cap bump, GE-INTERP-LAYOUT
  regression with the maker-self `wide-mark` STAND-IN.

OPEN:
- REMAINING (2) interpret-mode wide values — CLOSED 2026-07-09: checker half
  landed (commit "TFAM 12: checker-computed interpret wide marking"); engine
  half was already landed; the 12-interpret dot is closed and its two
  documented residuals moved to dot habu-interpret-wide-gate-1d70acf7.
- REMAINING (3) does>-split — OPEN (implementation plan below); (6) snapshot
  doctored-trailer fixture — OPEN.
- REMAINING (4) — DECIDED 2026-07-09 (document-the-vacuous-boundary, proven):
  no SUMTYPE/PRODUCT/TYPEFAMILY/ENUM declaration exists in src/, lib/, tools/,
  or maki/ non-test source (line-start declaration scan = 0 rows; hits are
  only the implementation words in src/core/sumtype.f and dispatch mirrors),
  so no definition compiled by a Gforth-recovered engine can carry a wide
  width fact before the immediate native fixpoint refresh replaces it
  (docs/bootstrap.md recovery contract). Mirror parity for pass-2 lowering +
  EM-REC-WIDE-PUBLISH is capability dot habu-bootstrap-mirror-pass-f1714953,
  which must land before (or with) the first non-test wide family declaration.
- REMAINING (5) — DECIDED 2026-07-09 (keep fail-closed, permanent): the
  checker reject already exists (HIDROW-STEP?, checker.f) with committed
  regressions (TD12-DEPTH/TD12-DOTS assert reject); docs §17 sanctions reject
  over logical-shape reporting. Comments settled at both sites; the lift is
  capability dot habu-logical-shape-depth-9686f5c1.

Gate tails for the items-4+5 verdict commit (2026-07-09, verbatim, true-rc):
- fixpoint refresh: `bin/hb refresh OK: compiler fixpoint` rc 0
- full gate: `PASS: native test suite (fixpoint + engine suite + checked hb +
  repl + hb-build) (27926ms <= 70000ms budget)` rc 0, zero RED lines
- test/type-decl-suite.f: `ok` rc 0
- dot-dep-lint: `164 dot(s), 13 blocker(s), 0 finding(s)` rc 0;
  typed-local-diff-lint on the diff: rc 0

REMAINING (3) — DECIDED + LANDED 2026-07-09 (keep fail-closed; labeled
engine exit; commit "TFAM 12: named reject for does>-split wide facts").
Probe evidence DISPROVED the earlier checker-side plan: the checked body
splits AT does> (the pass-1 hook checks only the create-part and the trigger
fires at publish), so the checker structurally cannot see that a does>
follows a wide-fact body — candidates see the full text but mark any does>
body UNCK (verdict 1, unmodeled token), and two OTHER fail-closed walls sit
in front (does> + locals dies 75 at "does>"; plain checked does> without a
captured does-sig dies 70 via C-DIE-DOES). The reachable raw exit was
EM-P2-TRIGGER's DOESB backstop printing only the current token — a lone ";"
(unattributable, the labeled-capacity-exit lesson). Landed: the exit now
writes `hb: does>-split cannot lower layout width facts: <token>` before rc
75 (habu2.f LP2DOESW), and GE-DOES-WIDE (test/gate-engine-lib.f, engine
runtime slice) pins rc 75 + the label for a wide dup/drop + create/does>
definition with no locals. Lifting needs two-phase-aware token indexing —
out of scope for v1, same class as the mirror parity dot.

Superseded plan (kept for history): the engine
fail-closes at EM-P2-TRIGGER (DOESB set + wf-wide? -> token write + exit
$4B/75, habu2.f) because pass-2's token indexing cannot align across a
does>-split body. Keep, but surface it as a CHECKER-side named reject BEFORE
the trigger: new flag (variable + reject in the does-aware verdict path when
WF-WIDE? and the body's does> token was seen + CHECK-RESET + CHECK-VERDICT +
render.f DCODE/REPAIR-CLASS/SUGGEST/PROSE rows, the same 5-site pattern the
E-LAYOUT-BRANCH-LOCAL guard used), negative regression in
test/type-decl-suite.f (wide fact + does> rejects; scalar does> stays
certified), engine exit kept as backstop. Fixpoint + full gate.

## Update 2026-07-09 (branch commits after the audit)

- Sub-dot 12-pass (branch-scoped bundle locals) — LANDED and CLOSED (commit
  "TFAM 12: pass-2 branch-scoped bundle locals"): checker LOCW-HW bind-sequence
  table + checker-hosted P2LW live replay (P2-CARVE-W/P2-LIVE-W@/P2-LIVE-CUM@/
  P2-LOCSEQ-RESET), P2-BRANCH-LOCAL-GUARD + render rows removed, TD12-BRLOC-*
  certified, TLPX-BR* execution rows run both arms (sibling slot reuse at
  widths 2 vs 4, mixed scalar+wide, outer+branch carve).

## REMAINING (1) resolved by verdict: fail-closed, no shape-carry (2026-07-09)

The staged carry design is REJECTED as unsound; the one-cell `-- a` model is
the PERMANENT `constant` contract. Rationale:
1. Physical ground truth: native C-CONSTANT pops exactly ONE cell (15 G-POP)
   and bakes one literal. Recording the producer's multi-cell type (staged
   PST-CONST-K-CARRY$: `CAE-CV-K | -- cae-cv`, 2 field cells) would let the
   checker certify USE words that push fewer physical cells than declared — a
   checker-certified stack corruption laundered through the constant.
2. No sound shape source: the interpret stack is untyped BY DESIGN (the
   12-interpret CHOSEN decision explicitly rejected typing the REPL as
   disproportionate). The only alternative — an adjacent-producer heuristic
   (`<word> constant NAME`) — mis-carries on `MK 5 constant K`, literals,
   stack shuffles, and multi-output words, and native C-CONSTANT would have to
   implement the same unsound inference to keep four-path parity.
3. The wide case is gated upstream: a wider-than-cell layout value can never
   LAND on the interpret stack (DNAME-WIDE dispatch gate — engine half landed,
   checker half = 12-interpret dot), so top-level `constant` can never see
   one; the gate at value PRODUCTION dominates every value-pop consumer. In
   checked bodies the pop rejects (TD12-CONST; docs/type-families.md §17
   sanctions "reject" as the alternative to "store the whole logical value").
   True multi-cell constant storage is buildable later without breaking this
   contract, but has no current consumer.

Landed as commit "TFAM 12: fail-closed constant layout pop": staged carry
fixtures DELETED (CAE-TEST-CONST-CARRY, PST-TEST-CONST-CARRY +
PST-CONST-K-CARRY$); the narrow parity fixtures re-commented as the permanent
contract (const-layout-narrow stays the runner row; TD12-CONST stays rejected);
the four sites' comments settled (C-CONSTANT habu2.f, verify-source
RECORD-DEFINER?, PS-MAYBE-TRUST-DEFINER, all-errors constant-capture comment —
CA-ADD-SUPPORT-CONSTANT no longer exists as a word; the funnel is
verify-source). The `-- a` trust rows themselves are unchanged (they ARE the
contract).

Gate tails for the verdict commit (2026-07-09, verbatim, all true-rc):
- fixpoint refresh (install --force): `bin/hb refresh OK: compiler fixpoint` /
  `bin/hb ready (small checked engine, tty REPL + stdin)` rc 0
- full gate `bin/hb --load test/run.f`: `PASS: native test suite (fixpoint +
  engine suite + checked hb + repl + hb-build) (30177ms <= 76300ms budget)`
  rc 0, zero RED lines
- tools/check-all-errors-test.f: `test: ok` / `check-all-errors-test: ok` rc 0
- tools/public-signatures-test.f: `test: ok` / `public-signatures-test: ok` rc 0
- test/type-decl-suite.f: `ok` rc 0; test/type-layout-lower-pending.f (stdin):
  `ok` rc 0; test/type-family-suite.f: `ok` rc 0; test/type-ctor-suite.f:
  `ok` rc 0
- maki/test.f: `test: ok` / `PASS: maki/device-smoke.f (7ms)` rc 0
- tools/dot-dep-lint.f: `dot-dep-lint: 162 dot(s), 13 blocker(s), 0
  finding(s)` rc 0
- tools/typed-local-diff-lint.f on the jj diff --git: rc 0