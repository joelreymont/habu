---
title: Move the interpreter into checked Habu
status: open
priority: 2
issue-type: task
created-at: "2026-09-29T12:51:36.389107+03:00"
blocks:
  - habu-capture-does-in-46068833
  - habu-close-definitions-with-8ace78d8
  - habu-hook-tier-0-96e33c29
  - habu-compile-definer-bodies-1292d049
  - habu-move-pkgs-using-22f18b81
  - habu-move-evaluate-and-9119f746
  - habu-roll-back-failed-64bf2ba5
  - habu-recover-throws-across-18bd36f9
  - habu-parse-argv-and-2106dd5c
  - habu-route-stdin-repl-d7e12895
  - habu-boot-the-arm64-d0d4421a
  - habu-add-the-engine-ebe5d757
  - habu-call-the-unit-ec148ccf
---

Lane I: the outer interpreter (`src/habu/outer.f`), definers (`definers.f`), packages (`packages.f`) and the `MAIN` entry (`main.f`) in checked Habu, captured into both product engines; a `prims.f` row kind marks rows whose body is a checked definition in a named prefix file. Spark; the ARM64 gate proves each step, so the interpreter is proven before x86 exists. Tier 0 on the ARM64 product: B1 (chosen, I6) keeps the JIT through `jit-open`/`jit-token`/`jit-close`; B2 (tier-1-only products on both arches) is a follow-on opened with G4b's numbers. I10c is the one leaf that changes the kernel boot on ARM64 (`EM-STARTUP`'s seeded branch calls `MAIN` through `ENGINE-MAIN:XT-CELL`); cold engines and the Gforth chain are unchanged. Run the periodic no-binary check after I6 and I10c. Serialise: `habu2.f` (K4, I6, I10c, X7); `aot-capture.f`/`aot-closure.f` (X2a, X2b, I7).
Leaves: habu-find-dictionary-names-e8f56969 (I2), habu-parse-numbers-in-86b27302 (I3), habu-scan-and-interpret-eea996a2 (I4), habu-move-tier-1-aacb6029 (I5a), habu-capture-bodies-and-5cbd31ea (I5b), habu-compile-immediates-from-cc47ecf4 (I5c), habu-capture-does-in-46068833 (I5d), habu-close-definitions-with-8ace78d8 (I5e), habu-hook-tier-0-96e33c29 (I6), habu-compile-definer-bodies-1292d049 (I7), habu-move-pkgs-using-22f18b81 (I8), habu-move-evaluate-and-9119f746 (I9a), habu-roll-back-failed-64bf2ba5 (I9b), habu-recover-throws-across-18bd36f9 (I9c), habu-parse-argv-and-2106dd5c (I10a), habu-route-stdin-repl-d7e12895 (I10b), habu-boot-the-arm64-d0d4421a (I10c).
Campaign lane only; do not dispatch. It lists its leaves under blocks: so it stays off dot ready until they close.
Ownership: krait (Intel lane).
Claim: unassigned.

Lead note (2026-10-01, I8 landed): the interpret loop (DISPATCH, STEP, RUN, INTERPRET) now lives in `src/habu/interpret.f` (package OUTER; requires `outer.f`, then `packages.f`), and the package keywords in `src/habu/packages.f`. `outer.f` keeps the scanner, the find and number steps, SEAL-GUARD, TASK-GUARD, FAIL-CLOSED and PROTECTED?. A child dot that cites `outer.f` for the loop means `interpret.f`. INTERPRET saves and restores USE-DEPTH on both exits; package-scope rollback on a throw is still `habu-roll-back-failed-64bf2ba5`'s.
