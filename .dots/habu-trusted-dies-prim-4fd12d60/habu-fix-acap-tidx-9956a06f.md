---
title: Fix ACAP-TIDX-INS spill rejection in optimizing selfbuild
status: closed
priority: 1
issue-type: task
created-at: "\\\"2026-09-11T16:59:27.075369+03:00\\\""
closed-at: "2026-09-11T17:21:56.092332+03:00"
close-reason: Reviewed b7780bba integrated as c0bd71d4; original reducer and EXHAUST/EXHAUST-READ controls pass; actual optimizing selfbuild passed ACAP-TIDX-INS and reached separate PF-LAYOUT-REQUIRE blocker8f0960e3.
---

Owner: cedar (/root). Combined reviewed source 92ef13f0 includes all three KEEP review repairs, earlier spill corrections, argv fix, FNV symbol filter and flat arena reads. Current-layout bootstrap checkpoint SHA256114c1c09c9c729c052fcc7a84763dbed2ecce424051857ecff04305dc675289b built in21.150s. Actual optimizing selfbuild from that frozen checkpoint, selecting test/compiler/aot-mode.f before tools/native-build.f, fails rc67 after95.392s: ncomp cannot compile ACAP-TIDX-INS, uncaught -8442 (spill check). Log /tmp/cedar-current-optimizing-selfbuild.log; cwd cedar-aot-selfbuild. No optimizing output was produced. Acceptance: reduce the exact valid function, identify and fix the responsible spill/frame invariant without disabling verification, add regression, independently review, then rerun actual optimizing selfbuild. The three earlier new KEEP gaps remain separate fixed cases; this is newly exposed by full selfbuild.


Reduced reproduction now takes0.464s on matched92ef13f0 checkpoint: /tmp/cedar-acap-reduce.f keeps actual index insertion body and required helper definitions, using16 index slots. Diagnostic source in cedar-selfbuild-spill pinpoints F-ORDER-EDGE! at original spill.f:614: two incoming paths carry different frame tokens into a destination without a synthetic frame argument. Investigating why the destination was exempted. Temporary numbered diagnostics are not a candidate patch and must be removed.


Update2026-09-11 14:15 UTC: Fixed by b7780bba, independently cleared by compiler_xhigh_review, integrated as c0bd71d4. TRAP-ORDER-UNUSED? requires a terminal TRAP, no planned store/reload, and no original frame access; only those blocks omit frame-order edge/entry bookkeeping. Original ACAP reducer0.543s and native-order-exit0.614s pass, including early return, zero/exhausted loops and EXHAUST-READ contrast that requires a reload. native-regalloc12.137s and loop-frame-order4.423s pass with all final verifier checks intact. Matching binary7eff3ae80a0eacb9826e613618eb13c82958ca9cff2eeebd51f55fabb54dd34d. Full optimizing selfbuild retry on combinedc0bd71d4 is running; checkpoint and build logs /tmp/cedar-current-checkpoint-2.log and /tmp/cedar-current-optimizing-selfbuild-2.log.
