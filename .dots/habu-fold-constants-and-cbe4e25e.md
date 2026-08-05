---
title: Fold constants and number values on the IR
status: active
priority: 2
issue-type: task
created-at: "2026-08-05T09:41:37.369599+02:00"
---

The chain has no redundancy elimination: after transitive inlining, repeated subexpressions and re-materialized literals survive to emission. Add to the frozen-IR pipeline (as a pass between elaboration and selection, verified by the existing freeze/canon machinery): constant folding with the source dialect's own trap discipline (a folded division by literal zero becomes the guard's refusal, never UB), algebraic simplification (x*1, x+0, x*2^n to shift), local-then-global value numbering over pure ops (the schema's PURE/TOTAL flags already say which), copy propagation, and the single highest-value special case — DIVISION BY CONSTANT to multiply-by-magic-number (derive the magic constants the standard way, prove them against the architecture division for the full input range in the test suite, both signed rounding directions). Acceptance: measured on the clang-column gaps — division-heavy rows close most of their gap; no answer moves anywhere; the pass is off-switchable for bisection and its output re-verifies under the module verifier.

Blocked by: habu-epic-hard-cut-a684f24d phases 1-6. Re-scoped: operate on typed frozen IR before selection; fold only operations whose trap/overflow/FP policy is explicit; start with repeated literals/subexpressions and constant division where wins are easiest to measure. Run DCE after inlining so copied unused values disappear.

UNBLOCKED (2026-08-05, user order): the optimization program proceeds NOW on the chain as it stands — the hard cut continues in parallel and is no longer a prerequisite. Standing acceptance for every optimization lane: name the corpus rows expected to improve BEFORE implementing; show the emitted instruction delta on them; every oracle answer preserved bit-for-bit incl. NaN; report BOTH gaps per touched row (chain-vs-clang closed, chain-vs-own-baseline gained) from tools/codegen-compare.f; re-pin the chain baseline with --update-chain only after the report is read; no regression on untouched rows outside a stated multi-objective trade. New instruction forms (madd/msub, ldp/stp, bitfields, ccmp, NEON) require Rocq rows in formal/Common/Insn.v with enc/wf/roundtrip before the emitter uses them — the CG-02 discipline, applied per-lane not deferred.

CONSTANT-CSE ATTEMPT 2, MEASURED AND NOT LANDED (2026-08-05, fold2). The
literal-CSE half of this dot is blocked on a register-allocator capability, and
the block is a measurement rather than a guess.

The transform works and is sound. A block-local memo of literal value to
ir-value-id at src/compiler/native/elaborate.f EMIT-LIT, cleared at all three
IR-BUILD:BEGIN-BLOCK sites, collapses repeated materializations. Modules built
with it freeze cleanly, which runs the whole structural verifier, and five of
the six native suites pass with it on. tools/codegen-compare.f over all 41 rows
reports four improvements, no regressions, "rows the new column costs more on:
none", 0 finding(s):

  row                            before  after   vs clang
  CODEGEN-CORPUS4:CALL-FAN-BIG       88     56   72 -> 40
  CODEGEN-CORPUS4:TINY-CALLEE        96     80   84 -> 68
  CODEGEN-CORPUS4:CALL-FAN           48     44
  CODEGEN-CORPUS4:CALL-LOOP-3       112    108   64 -> 60

CALL-FAN-BIG hits its predicted 56 exactly, and at 56 it is still a loss against
the old emitter's 36, so the known-loss fact survives with the smaller number.

What stops it is test/compiler/native-chain.f RSPILL-CASE, which throws
-8329 E-A64RA-PRESSURE. Collapsing the two literal 1s in NCH-RSPILL stretches
one constant's live range across the add chain, and in that case's deliberately
starved frame the routine then needs two spilled values where the case pins
exactly one. Minus one movz, plus a store, a load and a slot. The two results
are one result: CSE's benefit is live-range extension, and live-range extension
is what raises pressure, so no version of this transform keeps the byte win and
avoids the pressure. The resolution is constant rematerialization in the
allocator, filed as habu-rematerialize-constants-cdce9a24, and the literal-CSE
leaf habu-literal-cse-trips-7e6d67bb now waits behind it.

Nothing was landed rather than land a measured pessimization or widen the
RSPILL pin to match it. The full evidence, including the corrected root cause of
the -8042 the previous attempt reported, is in
habu-literal-cse-trips-7e6d67bb.

Claim: agent=remat workspace=.jj-ws/habu-fold-constants-and-cbe4e25e
