---
title: Parse numbers in Habu
status: active
priority: 2
issue-type: task
created-at: "2026-09-29T12:51:36.638255+03:00"
---

Problem: number parsing is the assembly `LNUM`; `bootstrap/cg/forth.fs:2290` documents its contract.
Acceptance: the `LNUM` contract (value, ok, range-refused), `$hex` and negatives in `src/habu/outer.f`; a differential test against `num-parse`.
Files: `src/habu/outer.f`, a differential test beside `test/outer-find.f`.
Verify: spark: the differential test against `num-parse`.
Depends: none in the lane (I1 was folded into I9a, `habu-move-evaluate-and-9119f746`).
Route: Alder (shared: src/habu/outer.f and the new test).
Ownership: krait (Intel lane).
Claim: agent=krait workspace=.jj-ws/habu-parse-numbers-in-86b27302.
Preflight corrections (these override the lines above where they differ):
- Contract: the product reader, `src/habu/habu1.f:4461-4581` and `3303-3327`. `bootstrap/cg/forth.fs:2290-2378` is the Gforth recovery mirror and differs: signed limit in `C-NUM-INT-STEP` (2331-2332), no integer range refusal in `C-NUM-INT-FINISH` (2354-2355), no float flag. Measured on engine `ff7987e4`: `9223372036854775808` is refused by the product and accepted wrapped by the mirror.
- The product reader: unsigned wrap latch (4499-4503); decimal magnitude INT64_MAX, with 2^63 allowed only negative, and hex modular negation (4524-4532); float finish on a last-char `.` (4514-4522). Floats are part of the contract: `.5`, `-.5` and `1.5` answer float true; `1.`, `.`, `-.`, `$1.5`, `1e5` and `1.2.3` are not numbers; `0.0000000000000000001` (19 fraction digits) is range-refused while `0.000000000000000001` is a float. `num-parse`'s second output is that flag (`src/habu/prims.f:546`), consumed by the checker (`src/core/checker.f:11770-11778`) and `EM-COMPILE-LITERAL` (`habu2.f:9496-9499`).
- Acceptance: `OUTER:NUMBER ( ptr u8 n -- n bool bool bool )` in `src/habu/outer.f` (`package OUTER`): value (the double's bits for a float), float?, ok, range-refused; value and float? zeroed when ok is false, as `BNUMPARSE` does. Optional `-`; `$` selects base 16 with a-f/A-F; decimal floats `-?d*.d+` computed with `s>f`, `f/`, `f+`, `fnegate` in `C-NUM-FLOAT-FINISH` order, with a `CAST: ( r -- n )` for the bits (no `f>bits` primitive exists, `prims.f:613-627`; `CAST: R>BITS ( r -- n )` is admitted and gives `num-parse`'s bits for `1.5`); the unsigned wrap latch, decimal magnitude, hex modular negation and the fraction/scale latch as range refusal.
- Load: test-only, `require src/habu/outer.f` from the test (precedent `test/aot-address-cells.f:7-11`); the manifest lint walks only the manifest closure (`tools/manifest-lint-core.f:5-7,266-280`). No manifest or `prims.f` row: that is I9a.
- Oracle: `BNUMPARSE` ANDs value and flag with ok and never exposes the range latch (`12a` and `9223372036854775808` both answer `0 0 0`). Range-refused is observed as `EM-INTERPRET-NUMBER` (`habu2.f:8365-8366`) exiting `LUNDEF` rc 70 without `LFIND`: a hostile name of that spelling is defined and `evaluate` still exits 70, as `test/compiler/integer-literals.f:71-97` checks (`HOSTILE-NAME` + `GE-EVAL-FORK-BAD 70`).
- Files: `src/habu/outer.f`, `test/outer-number.f`, `test/gate-stdlib-cases.f` (`SUITE outer-number`).
- Verify (spark or the ThinkPad under qemu): tuple equality with `num-parse` over the inputs of `test/compiler/integer-literals.f:39-53,100-112` and `test/compiler/native-feed.f:294-315`, plus `-$` `.` `-.` `-0` `-$0` `+1` `--1` `0x10` `$aBcDeF` `$ABCDEF` `1 2` `$12g` `-$-1` `9223372036854775808.5`; range-refused agrees with "hostile name defined, evaluate exits 70"; gate.
- Overlap: I2 also creates `src/habu/outer.f` (package `OUTER`) and adds a SUITE row; the integrator merges the two additions.
- Route: Alder (`test/gate-stdlib-cases.f`; `outer.f` is new and loaded only by tests).
