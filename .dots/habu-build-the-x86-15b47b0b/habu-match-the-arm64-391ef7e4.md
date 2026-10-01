---
title: Match the ARM64 NaN bits on x86-64
status: closed
priority: 3
issue-type: task
created-at: "2026-10-01T08:00:12.728177+03:00"
closed-at: "2026-10-01T10:02:23.190374+03:00"
close-reason: "x86 selector clears the sign of a NaN made from numbers; prim-float-cases RR-R/R-R pass on ARM64 prim-parity and native hb-x64-kernel-pure (exit 0)"
---

Problem: a NaN the float ops create has a different sign on the two targets. SSE answers the default NaN $FFF8000000000000 (sign set) for invalid operations; ARM64 answers $7FF8000000000000. Measured through the x86 kernel rows (K13 review): `-1 fsqrt f.`, `0 0 f/ f.` and `inf inf f- f.` print `-0.000000` on x86 and `0.000000` on ARM64, since `f.` prints `-` iff bit 63 is set (`habu1.f` BFDOT). Any program that reads a computed NaN's bits (`f.`, bit casts, hashing, comparison of bit patterns) sees the difference. The word model states no NaN bit pattern, and `test/prim-float-cases.f` leaves NaN semantics to `lib/float-test.f`.
Acceptance: one rule for both targets, stated in the word model and docs: either every float op answers the same canonical NaN on both targets (the x86 selector canonicalises an invalid result, or ARM64 matches x86), or `f.` and the documented observables are NaN-sign-blind. Pick the rule that costs least on the hot float paths and keeps both engines' existing outputs where they are already pinned.
Files: `src/compiler/native/select-x64.f` (or the word model and `f.` rows if the rule is sign-blindness), `docs/x86-64.md`, a float case in `lib/float-test.f` or the parity sets that pins the chosen rule on both targets.
Verify: the three cases above print the same bytes on both targets (ARM64 under the engine, x86 through booted kernel images).
Route: direct if x86-only; engine lane if the rule changes the ARM64 side.
Ownership: krait (Intel lane).
