---
title: Plan combine once and fix measured spill scaling
status: closed
priority: 2
issue-type: task
created-at: "\\\"\\\\\\\"2026-09-11T16:38:06.270563+03:00\\\\\\\"\\\""
closed-at: "2026-09-16T14:34:49.484485+03:00"
close-reason: "superseded by habu-campaign-c1-finish-1f129a00: Measured spill scaling is a live plan item behind correctness; keep the measurement and the fixed point, not the standing lane."
blocks:
  - habu-walk-the-dynamic-e03edf85
---

Plan: [PLAN.md](../../PLAN.md). Design reconciled 2026-09-13; replaces stale diagnosis/claim. Claim: Cedar, `.jj-ws/cedar-combine-plan`.

Own native/combine.f/spill.f and focused tests/chain-scale cases. Rebase172fc17d on current lifecycle; duplicate REWRITES/FUSIONS versus REWRITE planning is defect, module already rebuilds at most once. Seal plan for exact input, consume once, invalidate release/refusal, preserve fold precedence/use/order/immediate checks. Keep four spill maps/descending work already landed. Count F-NEED visits on all-AOT baseline; if material use once-enqueued predecessor reachability for Boolean fixed point. Preserve allocation/rewrite fixed point and validator. Test stale/no-plan, no-change identity, real/call/quotation/control/KEEP spills and negatives. Require established slopes<=1.1; load12 reading is invalid.

Verification: focused real-load cases above; rebuild and run `bin/hb --load test/run.f` for compiler/runtime integration. Speed acceptance uses the all-AOT campaign pair; functional/count evidence can be developed in parallel.

Review 2026-09-13, snapshot51546316: Current review F02 confirms duplicate analysis in REWRITES/FUSIONS and REWRITE; the module is already rewritten at most once. Retain the no-op module and one-use plan for the exact input. F04: lowering/allocation is a required fixed point; no arbitrary one-pass/two-round truncation.

Candidate rebased onto 08b27a1b: decisions, including unused stack loads, are
planned once across the exact module and consumed by REWRITE. Reset, release and
refused rewrites discard the plan. Actual source-compiler native-combine and
native-regalloc suites pass; the latter exercises a nonempty plan discarded by
reset, source-digest refusal followed by success, and a refused second use.
The source compiler was JIT-compiled for these behavioral controls. Independent
review, all-AOT timing/slopes and F-NEED spill measurements remain open.

2026-09-14 follow-up, Cedar, `.jj-ws/cedar-select-combine`: the one-plan change
is integrated. One warmed actual trivial compile plans once and rewrites once;
the seven selected operations become six. Recovery product `5ab65284`, SHA
`83e0b0ea4c4b13f4d6470c4c43372c674a9b0069dc0b10fba7269941e453bb81`, has
identical compiler/IR source to this follow-up's base `b8e069a5`.

The existing per-definition driver, with disposable subphase clocks, attributes
314/242 us of a 953 us trivial compile to selection/combine. Binding their
downstream dialects costs 59/46 us; combine also takes 41 us for its builder,
78 us walking/copying and 42 us freezing. Exact GDB counts inside one warmed
compile show 536 A64IR memo bindings. Each hit resolves the same builder through
MODULE@, MODULE-KEY and SYMBOL-CK and reopens the live symbol header. The 402
discarded NTH conversions account for only about 1.5 us in a calibrated probe,
so they are not the floor fix. Raw evidence is under
`/tmp/cedar-select-combine-{count,pass-0,deep,samples,ordinal-cost}.log`; clocks
and counts exclude startup and dictionary cleanup. These are attribution runs
under concurrent engine work, not quiet timing or 500 us acceptance.

Production `54fad493` binds the full opcode vocabulary as one checked batch in
combine, spill and emit. IR-BUILD resolves the live builder and exact module;
IR-SYM validates the current header/key and every ordinal before copying any
identity. Missing, partial or mismatched memos still use the existing interner.
No borrowed arena state survives the call or an append. Public per-entry
bindings, dialect keys/types, combine planning and freeze validation are unchanged.
M (`99caf411`) built that source successfully as
`/tmp/cedar-family-stage-abi/hb-opcode-batch`, SHA
`6a29d0618063dbd36b0d138804cad63618315be101f6d318e0b937fe70a66da0`.
Forced-tier-1 ir-build, native-a64ir, native-select, native-combine and
native-regalloc suites pass on the product, including real spill rewriting.
The full ir-symbol suite passes with ordinary fixture loading. Its unchanged
SCALE-CASE refuses forced-tier-1 compilation with E-NELAB-ARITY on both candidate
and the prior recovery product; the new IDS controls also pass independently
as a forced-tier-1 excerpt. No existing assertion was changed for that refusal.
Logs are `/tmp/cedar-batch-{ir-build,native-a64ir,native-select,native-combine,
native-regalloc,ir-symbol-default,symbol-controls,baseline-symbol}.log`.

The same one-compile GDB interval now counts 156 A64IR memo bindings (88 select,
68 combine), exactly 380 fewer; SYMBOL-CK and LEN@ each fall by 380. There are
five owned batches, one combine plan/rewrite, and the same three builders and
freezes. `/tmp/cedar-batch-count.log` records the counts. Independent Astra
source and native behavior review approved production and tests. Root integrated
them as `e73c85f6`/`f1528833`.

For an attributable current-product pair, the same delta was duplicated onto
M (`99caf411`) as `e474055d`/`94a118c1`. M built it successfully as
`/tmp/cedar-family-stage-abi/hb-opcode-batch-M`, SHA
`46a9d66884fafba495d309ddd1ca077fabee466798a10c31f3c9a14c7819ed3c`.
The build took approximately 135.5 s from log creation through final promotion;
a monotonic whole-build timer was not captured. Three interleaved stock
`tools/compile-floor.f` pairs measured M→candidate trivial
983→925, 981→925, 980→928 us, and three-op 642→611, 647→612, 642→610 us.
Every run compiled exactly 200 definitions; JIT remained 30–31 us. Load was
0.90 throughout and the initial process snapshot contained no competing hb.
`/tmp/cedar-batch-M-pairs.json` records source identities, process/load state,
and raw output. Mean savings are 55 us (5.6%) trivial and 33 us (5.1%) three-op.
The 500 us target remains open.
