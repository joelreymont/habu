---
title: Package the engine gate vocabulary, then scale its wall
status: open
priority: 2
issue-type: task
created-at: "2026-08-07T15:02:15.833117+02:00"
---

test/gate-common-lib.f defines GE-TIMEOUT-MS as a FIXED 120000 constant - the same failure class STDLIB-GATE:SUITE-TIMEOUT-MS had, where a per-suite wall in a process-spawning gate cannot tell a slow box from a hung child (compiler-insn-proof, 99543ms quiescent, killed at 120145ms under a second concurrent gate on 2026-08-07). The stdlib side is fixed: SUITE-TIMEOUT-MS now derives from a nominal through lib/test/budget.f T-BUDGET-MS, and test/run-lib.f PHASE-BASE hands HB_LOAD_PCT/HB_CAL_PCT to every spawned phase, so a spawned child scales the same way a resident one already did. The engine side is NOT, and the blocker is ownership, not design: every word in test/gate-common-lib.f is global, so turning GE-TIMEOUT-MS into a word reds package-diff-lint with two E-PACKAGE-OWNERSHIP findings (measured 2026-08-07: 'GE-TIMEOUT-NOMINAL-MS defines a changed module word outside a package', same for GE-TIMEOUT-MS), and the file is on no global-exception list in docs/forth.md. Give the file a real package - roughly 20 GE-* call sites across test/gate-engine-lib.f, gate-aot-positive-lib.f, gate-dictionary-lib.f, gate-debug-lib.f - and then make the wall load-aware exactly as the stdlib one is, with the claims added back to test/gate-budget-test.f (they were written and removed in the same change; see its header).
