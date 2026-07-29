---
title: Package bootstrap-codegen-test and judge fixtures
status: active
priority: 2
issue-type: task
created-at: "\"2026-07-30T00:46:44.515804+02:00\""
---

Full context: last package-gate blocker for landing the stage0 using commit (b9d5fca5) and the recovery prologue fix (88d258e1) in .jj-ws/habu-add-using-to-d815f0ab. Two finding classes on their combined artifact. (1) tools/bootstrap-codegen-test.f defines ~30 global BCG-TEST-* words plus BCG-MAIN with no package owner - it is an ordinary native test tool run by bin/hb, so packaging IS possible and raw global stems are forbidden by AGENTS.md; give it a real package with short tails and update any callers (check how tools/bootstrap.sh or suites invoke it - the entry word's name may be pinned somewhere; measure before renaming). (2) test/bootstrap-using-*-src.f fixture sources are compiled ONLY by the stage0 recovery engine inside tools/bootstrap.sh; the checker-hook fixture NEEDS a global CHECKER-USING because the engine finds it by bare lookup (an engine contract), and the shadow/caller fixtures deliberately prove TOP-LEVEL bare visibility - packaging them would destroy what they test. Decide the principled treatment: either package what can be packaged without changing the tested property, or add an exact-path-family category for stage0 recovery fixtures (same one-comparison-site row pattern as the mirror category, commit 2cceebce; justification: the fixture IS the gate's own input and its correctness authority is the bootstrap gate's whole-stream comparisons). Pin both directions in tools/package-diff-lint-test.f and falsify by mutation. Acceptance: the combined artifact of both stacked commits plus this one reports ZERO package findings; bootstrap-codegen-test.f exit 0 through its owning path; the bootstrap using gate fixtures still pass.

Claim: agent=bcgpkg workspace=.jj-ws/habu-add-using-to-d815f0ab
