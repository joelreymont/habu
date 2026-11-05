---
title: Move the interpreter into checked Habu
status: open
priority: 2
issue-type: task
created-at: "2026-09-29T12:51:36.389107+03:00"
blocks:
  - habu-compile-definer-bodies-1292d049
  - habu-move-pkgs-using-22f18b81
  - habu-move-evaluate-and-9119f746
  - habu-roll-back-failed-64bf2ba5
  - habu-recover-throws-across-18bd36f9
  - habu-route-stdin-repl-d7e12895
  - habu-boot-the-arm64-d0d4421a
  - habu-add-the-engine-ebe5d757
  - habu-call-the-unit-ec148ccf
---

Lane I: the outer interpreter (`src/habu/outer.f`), definers (`definers.f`), packages (`packages.f`) and the `MAIN` entry (`main.f`) in checked Habu, captured into both product engines; a `prims.f` row kind marks rows whose body is a checked definition in a named prefix file. Spark; the ARM64 gate proves each step, so the interpreter is proven before x86 exists. Tier 0 on the ARM64 product: B1 (chosen, I6) keeps the JIT through `jit-open`/`jit-token`/`jit-close`; B2 (tier-1-only products on both arches) is a follow-on opened with G4b's numbers. I10c is the one leaf that changes the kernel boot on ARM64 (`EM-STARTUP`'s seeded branch calls `MAIN` through `ENGINE-MAIN:XT-CELL`); cold engines and the Gforth chain are unchanged. Run the periodic no-binary check after I6 and I10c. Serialise: `habu2.f` (K4, I6, I10c, X7); `aot-capture.f`/`aot-closure.f` (X2a, X2b, I7).
Leaves: habu-find-dictionary-names-e8f56969 (I2), habu-parse-numbers-in-86b27302 (I3), habu-scan-and-interpret-eea996a2 (I4), habu-move-tier-1-aacb6029 (I5a), habu-capture-bodies-and-5cbd31ea (I5b), habu-compile-immediates-from-cc47ecf4 (I5c), habu-capture-does-in-46068833 (I5d), habu-close-definitions-with-8ace78d8 (I5e), habu-compile-definer-bodies-1292d049 (I7), habu-move-pkgs-using-22f18b81 (I8), habu-move-evaluate-and-9119f746 (I9a), habu-roll-back-failed-64bf2ba5 (I9b), habu-recover-throws-across-18bd36f9 (I9c), habu-parse-argv-and-2106dd5c (I10a), habu-route-stdin-repl-d7e12895 (I10b), habu-boot-the-arm64-d0d4421a (I10c).
Campaign lane only; do not dispatch. It lists its leaves under blocks: so it stays off dot ready until they close.
Ownership: krait (Intel lane).
Claim: unassigned.

Lead note (2026-10-01, I8 landed): the interpret loop (DISPATCH, STEP, RUN, INTERPRET) now lives in `src/habu/interpret.f` (package OUTER; requires `outer.f`, then `packages.f`), and the package keywords in `src/habu/packages.f`. `outer.f` keeps the scanner, the find and number steps, SEAL-GUARD, TASK-GUARD, FAIL-CLOSED and PROTECTED?. A child dot that cites `outer.f` for the loop means `interpret.f`. INTERPRET saves and restores USE-DEPTH on both exits; package-scope rollback on a throw is still `habu-roll-back-failed-64bf2ba5`'s.

Ruling (Joel, 2026-10-23): one interpreter; ARM64 keeps tier 0 beside tier 1. The 2026-10-08 one-codegen rule and its tier-0 retirement are withdrawn.

Joel 2026-11-04: "we decided to unify the frontend and just have different code-generation backends. why the fuck do arm64 and x86 have different interpreters? why does arm64 not use the typed one if it's better? why are we patching and working around instead of doing what's best for long term?" State that day: x86-64's product runs the typed loop (src/habu/runtime-x64.f:19-24, loaded only for x86-64 by native-runtime.f:183-186); arm64's product still runs habu1.f/habu2.f's machine-code interpreter (B-EVAL, B-EVAL-RANGE, B-EVAL-CLOSED habu1.f:1827-1838, :3932; policy-admit's LROWWALK walk; its own create at habu2.f:8952; its own startup, I10c). The arm64 switch is folded into the cross-build design (brief /tmp/jerry-big/xbuild-plan-prompt.md, output /tmp/jerry-big/design-xbuild.md; dot habu-make-a-file-dfc7e030): the arm64 product takes the x86-64 product's shape (primitive kernel, typed runtime, written by the linker) instead of habu2.f's entries being patched to jump through cells one by one.

Acceptance cases folded in by the 2026-11-04 census (the closed dots' full text is in the history before the census commit):
- habu-locate-loop-refusals-0754c0b1 (closed): habu2.f LDIAGRET refusals unlocated; located loop refusals become a case of the one typed interpreter [v]
- habu-name-a-refused-6e9b9843 (closed): habu2.f LCOMPILEDIE prints only "does> at"; diagnostic for a refused clause is an acceptance case; same as 6848c356 part (c) [i]
- habu-check-top-row-997dd86e (closed): test now test/compiler/native-eval.f:302; x86 kernel prints the same "hb: interpret stack underdepth: " (src/habu/kernel-x64.f:381); x86 run not done, inferred
- habu-clear-the-addr-23daaaee (closed): habu2.f EM-EVAL-THROW-RECOVER leaves CALLMAP/ADDRMAP rows; a failed evaluate rolls back fully: acceptance case of the typed interpreter
- habu-checking-pair-or-a859fbfe (closed): "lowering certificate replay drift" is habu2.f:5868 DRIFT$ (verified); pair/sum locals beside a quotation must check and run after M1
- habu-keep-prot-wid-3dd32ed9 (closed): "A seal protects any wordlist id" holds after; readers EM-AOTWIDGATE, capture and snapshot restore are habu2.f and window machinery.
- habu-name-the-file-749de7af (closed): uncaught throw during load lacks file:line; fix sites habu2.f LCOMPILEDIE, stage2.f RUN, driver-io.f are M1/M2; must hold in the one typed loop
- habu-refuse-removing-a-39fa31f5 (closed): reserved-by-name packages are habu2.f RESTAB-BUF :2019 marks; "removing a word of a sealed package" must hold in the replacement
- the arm64 assembler (DECISIONS 2026-11-05): habu2.f:735-745 encodes bl at run time with its own BL-REACH window and opcode; the checked interpreter calls A64ASM:ENC-BL and keeps no reach check of its own
- a compiled definer's `create` under an arm64 native shadow (S6 L3a): habu2.f's machine-code create (ADDR-SHADOW,) asks tier 1's SHADOW-FIXED for the shadow copy, which throws E-COMP-STATE for any fixed body but a zero cast (src/compiler/native/compiler.f:718-723); with the typed definer in CREATEP-CELL it files one; arm64-place.f and shadow-kinds.f call OUTER:INSTALL-CREATE for it, which the one typed interpreter makes the default
