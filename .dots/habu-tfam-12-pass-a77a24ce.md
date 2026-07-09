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

CONFIRMED codegen mechanism (2026-07-09, decisive for the fix shape):
EM-P2-QUERY-LOCW/-LOCWCUM (habu2.f:2286-2296) EMIT runtime code — `C-FIND-GLOBAL`
(:1259) emits an LFIND + puts the xt in x11, `C-CALL-X11-SAVED` (:1215) emits
`BLR x11`. So EM-P2-CARVE (:2307) and EM-P2-LOCREF (:2354) bake a RUNTIME loop into
the defined word's prologue that calls locw@/locw-cum@ AT THE WORD'S RUNTIME,
indexed by LIVE local index [P2LOC0..LOCN). LOCW is per-CHECK scratch, so this is
sound only because these wide-local words execute IMMEDIATELY after definition
(same window LOCW is valid) — the execution rows in type-layout-lower-pending.f
all define+run inline. Implication for this dot: LOCW-HW and P2LW are likewise
RUNTIME tables (DATA cells) read at word-runtime. The fix = pass-2's emitted carve
increments a runtime P2-LOCSEQ per `{:` group and fills P2LW[live-index] from
LOCW-HW@(seq) so a reused live slot (after a branch join) gets the NEW local's
width via its NEW bind-seq; EM-P2-CARVE base-slot (`LOCF/8 - locw-cum@(i)`) and
EM-P2-LOCREF read P2LW instead of live-index LOCW@/LOCW-CUM@. Checker: LOCW-HW[seq]
appended at LOC-ADD (checker.f:5195, alongside `1 #LOC cells LOCW !` at :5204) and
LOC-BUNDLE-BIND (:5241, alongside the LOCW store at :5244), NEVER rewound by
CF-LOC-REST; LOCSEQ reset in CHECK-RESET (:6140-6148). Add LOCW-HW@ PRIM row next to
LOCW@/LOCW-CUM@ (:3969). This is a byte-fixpoint engine commit (checker.f + habu2.f)
+ full gate + inline-execution proof across both branches; NOT started to avoid a
half-landed silent-miscompile risk.
