---
title: Wire device-LN backward + execution
status: closed
priority: 2
issue-type: task
created-at: "\"\\\"2026-07-20T23:45:52.296293+02:00\\\"\""
closed-at: "2026-07-21T07:41:15.427120+02:00"
close-reason: "Landed in stack cb1e4cae: plain LayerNorm forward AND closed-form backward device-lowered to the GB10 (tools/ptx/layernorm-cg.f from the existing M6 row/collective vocabulary - no new lib ops; reuses RMS-EPS+ since LN and RMS share the bit-identical 1e-5 eps, documented). Device proof on sm_121a: forward matches host LN-FWD to 6 decimals; backward matches host LN-BWD closed form to 6 decimals AND central FD to <=5e-5. Affine handled honestly: this kernel pair is plain-LN-only by signature (affine fwd already lowered in LRED, affine param grads are column reductions - a different axis - recorded boundary, LRED already fail-closes). Registered per the rmsnorm/rope pattern incl. perf-watch producer row + WAIVER perf rows, kernel-perf lint proven load-bearing by stripping them. Loose ends: EMIT-REDUCE WAR-hazard hardening dotted; Orin goldens folded into the standing Orin-owed dot"
---

Unowned gap surfaced by the inventory rebuild (03834011): LayerNorm's backward (LN-BWD/ROWSUM-BWD) is not device-lowered, and even forward device execution was blocked by the harness never setting the ptxas arch (the E-PTXTC-ARCH class). The arch half is now solved twice over: the ATGT probe idiom (rmsnorm/rope device tests) and the TC-GATE identity landing. Own the remainder: lower LN backward to device, execute forward+backward on the GB10 through TC-GATE, parity element-close vs host, gradcheck on-device path. Files: maki/lower/* LN legs + device test; no src/.

Claim: agent=devln workspace=.jj-ws/fable-devln machine=spark (owns device LN backward lowering + GB10 execution parity: maki/lower LN legs + device test; correctness-only GPU)
