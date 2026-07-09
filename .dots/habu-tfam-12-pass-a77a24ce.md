---
title: "TFAM 12: pass-2 branch-scoped bundle locals"
status: open
priority: 2
issue-type: task
created-at: "2026-07-06T22:20:58.090967+02:00"
---

Lift E-LAYOUT-BRANCH-LOCAL (src/core/checker.f P2-BRANCH-LOCAL-GUARD / CHECK-VERDICT; render.f DCODE/REPAIR-CLASS/SUGGEST-TEXT/DIAG-PROSE). Today the checker REJECTS any local bound in control-flow branch scope (if/case/loop) in a definition that triggers the width-aware pass-2 recompile (WF-WIDE? true). Root cause: pass-2 (habu2.f EM-P2-CARVE/EM-P2-LOCREF via EM-P2-QUERY-LOCW/-LOCWCUM -> checker LOCW@/LOCW-CUM@) reads the per-CHECK LOCW table AFTER the hook certifies, but branch-scoped locals are popped from #LOC at their join (CF-LOC-REST, checker.f:5324) and the scalar emitter REUSES their frame slots (LCFPUSH/LCFPOP save+restore LOCN/LOCF, habu2.f:842-859). So a branch-scoped local is read out of range (die 76) OR its slot is reused by a sibling/later local with a different width (silent miscompile). A whole-definition high-water width table indexed by bind order does NOT match this frame math. Correct fix (this dot): give pass-2 a per-bind-occurrence width source. Checker records each local's final width keyed by a monotone bind sequence (never popped) into a new LOCW-HW[seq] table + a LOCW-HW@ query; pass-2 maintains a parallel P2-LOCSEQ counter and its OWN live width table P2LW[live-index] filled at each carve from LOCW-HW@, reading P2LW for both the carve cumulative and the local reference (LLOC-FIND live index) so frame reuse is handled position-correctly. Then EM-P2-CARVE/EM-P2-LOCREF stop calling LOCW@/LOCW-CUM@ by live index post-check. Acceptance: TD12-BRLOC-IF/CASE/MIX/SCALAR (test/type-decl-suite.f) flip from rejected (0) to certified (-1); new execution rows in test/type-layout-lower-pending.f prove a wide local bound and referenced inside if/else and case/of arms lowers and RUNS correctly across both branches, plus a sibling-branch reuse case (different widths at the same LOCN slot) and a mixed scalar+wide branch group; remove the P2-BRANCH-LOCAL-GUARD reject.

## Audit vs tip db88a576 (2026-07-09): OPEN (nothing landed)

- checker.f: P2-BRANCH-LOCAL-GUARD present at :6207 (WF-WIDE? && LOCBRANCH), fired
  from CHECK-VERDICT :6274; LOCBRANCH set in LOC-ADD :5208 when #CFC>0; LOCW table
  at :5125 with LOCW@ :5133 / LOCW-CUM@ :5136 keyed by LIVE index (LOC-IX-GUARD dies
  76 out of [0,#LOC)). No LOCW-HW / LOCW-HW@ / bind-sequence table yet.
- habu2.f: EM-P2-CARVE / EM-P2-LOCREF read LOCW@ / LOCW-CUM@ by live index; no
  P2-LOCSEQ counter or P2LW live-width table.
- render.f: P2-BRANCH-LOCAL-GUARD DCODE / REPAIR-CLASS / SUGGEST-TEXT / DIAG-PROSE
  rows present.
- tests: TD12-BRLOC-IF/CASE/MIX/SCALAR in test/type-decl-suite.f assert rejected (0);
  test/type-layout-lower-pending.f references the guard, no wide-local execution rows.
- Reference frame math to preserve: CF-LOC-REST pops branch locals from #LOC at the
  join (checker.f ~:5421,:5541+); scalar emitter reuses frame slots via LCFPUSH/LCFPOP
  (habu2.f). LLOC-FIND resolves a local reference to a LIVE index; the pass-2 fix must
  drive P2LW by the SAME bind order the checker records into LOCW-HW[seq].

## LANDED (2026-07-09, commit "TFAM 12: pass-2 branch-scoped bundle locals")

Mechanism correction to the earlier audit note: the EM-P2 width queries are NOT
word-runtime calls. EM-P2-CARVE/EM-P2-LOCREF are engine COMPILE-LOOP code — the
C-FIND-GLOBAL + BLR x11 they contain executes during the pass-2 recompilation
(immediately after the pass-1 hook certifies, while per-CHECK scratch is valid)
and the results are BAKED into the user word as constant frame-slot offsets.
The bug was purely live-index skew: the checker pops #LOC at joins (CF-LOC-REST)
while the engine replays the body with its own LOCN, so live-index LOCW reads
died 76 or read a sibling arm's width.

Fix as landed (storage checker-hosted — the DATA map has no free 65-cell hole;
$3A00..$3C88 is the lib/ffi-abi.f FFI block — so the pass-2 live table lives
beside the width facts it derives from, zero new engine DATA cells):
- checker.f: LOCW-HW[LOC-HW-CAP = 4*LOC-CAP] final width per bind occurrence,
  keyed by monotone LOCSEQ (assigned in LOC-ADD beside the LOCW store, cap
  folded into the over-cap LOCALBAD reject; finalized via LOCSEQIX[live] in
  LOC-BUNDLE-BIND); never rewound by CF-LOC-REST; LOCSEQ reset in CHECK-RESET.
  Queries: LOCW-HW@ (die 76 past LOCSEQ = misalignment backstop) + pass-2 live
  table P2LW[LOC-CAP]/P2SEQ with P2-LOCSEQ-RESET / P2-CARVE-W (consumes next
  seq, records live width) / P2-LIVE-W@ / P2-LIVE-CUM@. PRIM rows replace the
  LOCW@/LOCW-CUM@ rows; live-indexed LOCW@/LOCW-CUM@/LOCW-IX-GUARD deleted.
  P2-BRANCH-LOCAL-GUARD, LOCBRANCH, P2BRLOCBAD deleted (CHECK-RESET +
  CHECK-VERDICT rows too).
- render.f: E-LAYOUT-BRANCH-LOCAL DCODE/REPAIR-CLASS/SUGGEST-TEXT/DIAG-PROSE
  rows removed (one trailing THEN off the DCODE cascade).
- habu2.f: EM-P2-CARVE-W / EM-P2-LIVE-W / EM-P2-LIVE-CUM replace
  EM-P2-QUERY-LOCW/-LOCWCUM (same emit shape; name labels LP2CARVW/LP2LIVEW/
  LP2LIVEC/LP2SEQRST in EMIT-P2KW + EMIT-LABEL-P2); EM-P2-TRIGGER calls
  p2-locseq-reset before EM-P2-START (RX window already open there). The carve
  width pass consumes one seq per group local in textual order = checker
  LOC-ADD order (LOC-BEGIN rejects {: in dead code and inside quotations, so a
  certified body cannot diverge from the replay).
- tests: TD12-BRLOC-IF/CASE/MIX/SCALAR flipped to certified (-1);
  test/type-layout-lower-pending.f gains define+run-inline rows TLPX-BRIF,
  TLPX-BRCASE (case/of arms), TLPX-BRW (sibling arms reuse frame slot 0 at
  width 2 vs 4), TLPX-BRMIX (scalar+wide group in an arm), TLPX-BROUTER
  (outer w4 local below a branch-scoped w2 local; survives the join) — each
  runs BOTH arms. prop-test census AX-NOEXEC-C rows swapped accordingly.
- Emitted-instruction goldens unchanged (the fix redirects the compile-time
  width source only; emitted user code is identical for previously-legal
  bodies).

Boundary found while writing the rows (pre-existing, NOT this dot): a RAW
generated-constructor call followed immediately by a local bind rejects at the
:} (`7 TLP--RES:ERR {: r :}` — ctor output flows through the CTOR-PEND
signature-boundary coercion, not yet a bindable row group; fails identically at
top level, unrelated to branches). The suite's checked maker-word seeds
(TLP-MK2/TLP-MK2B) are the supported surface; constructor mid-body ergonomics
belong to items 8/9 (see habu-retire-tlp-mk2-ac7760d2).

Gate tails (2026-07-09, verbatim, all true-rc):
- fixpoint refresh (install --force): `bin/hb refresh OK: compiler fixpoint` /
  `bin/hb ready (small checked engine, tty REPL + stdin)` rc 0
- full gate `bin/hb --load test/run.f` (final tree): `PASS: native test suite
  (fixpoint + engine suite + checked hb + repl + hb-build) (9667ms <= 40000ms
  budget)` rc 0, zero RED lines
- test/type-decl-suite.f: `ok` rc 0
- test/type-layout-lower-pending.f (stdin): `ok` rc 0
- test/type-family-suite.f: `ok` rc 0
- test/type-ctor-suite.f: `UNDEF-SAFE` / `ok` rc 0
- maki/test.f: `test: ok` / `PASS: maki/device-smoke.f (1ms)` rc 0
- test/prop-test.f (stdin): `prim-axiom: census OK (every PES axiom
  classified; executable axioms difftested)` / `prop-test: sweep OK — 8` rc 0
- tools/dot-dep-lint.f: `dot-dep-lint: 163 dot(s), 13 blocker(s), 0
  finding(s)` rc 0
- tools/typed-local-diff-lint.f on the jj diff --git: rc 0
