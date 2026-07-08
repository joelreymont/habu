---
title: "RED: fold Rx1 column-broadcast operand into row kernel"
status: open
priority: 2
issue-type: task
created-at: "2026-07-07T07:30:53.399775+02:00"
---

lower-red.f LRED-CLASSIFY-INS currently fails closed (E-LRED-BCAST) on an Rx1 (BC-COL) broadcast operand: the block-per-row kernel loads a full RxC operand as base + row*k*4 (EMIT-ROW-SPAN, stride k=C), and a broadcast 1xC/1x1 pins to row 0 (EMIT-ROW-SPAN0). An Rx1 column operand instead needs base + row*1*4 (stride-1 row span) with a zero column ctx so every lane in block r reads element r (= e/C), matching executor EX-BC@ Rx1. Not a legal capture class today (cad.f SHP-LEGAL? only allows BIAS 1xC and SCALE 1x1/same), so no model produces it; the guard is defense-in-depth. To land: add an EMIT-ROW-SPAN-STRIDE1 (or param the stride) in lib/ptx/cg-collective.f, route BC-COL through it + EMIT-ZERO-OFF in LRED-LOAD-IN, convert the lower-red-test.f COL fail-closed fixture to a positive test, and add a device golden. Covered fail-closed by the COL fixture (maki/lower-red-test.f).

LANDED 2026-07-08 (host leg):
- lib/ptx/cg-collective.f: EMIT-ROW-SPAN-STRIDE1 ( base row -- span ) beside EMIT-ROW-SPAN0 - rowbase = cvta.global(base) + row*4 (mul.wide directly off the row reg, no row*k mul.lo). Chosen over parameterizing EMIT-ROW-SPAN: the file's existing pair already splits by span shape (full vs pinned), and a stride literal in EMIT-ROW-SPAN would ride every FULL call site for one caller's benefit.
- maki/lower-red.f: LRED-LOAD-IN routes BC-COL through EMIT-ROW-SPAN-STRIDE1 + EMIT-ZERO-OFF (every lane in block r reads element r, = EX-BC@ [e/C]); LRED-CLASSIFY-INS now rejects only BC-ILLEGAL - plus a NEW guard: input 0 must be BC-FULL, because LRED-BODY hardwires the full data-operand row span for input 0. Without that guard, unlocking BC-COL would have let a hand-built plan whose FIRST collected input is a broadcast silently emit a full-row span over a 1-column buffer (the same latent hole existed for BC-ROW/BC-SCALAR in position 0). Header/comments updated.
- maki/lower-red-test.f: COL fail-closed fixture converted to a positive PTX-text test (PTX-CAPTURE-ON; asserts the exact stride-1 chain mul.wide.u32 %rd9, %r2, 4 / cvta %rd10, %rd2 / add %rd11, and `, %r2, 4;` occurring EXACTLY ONCE proves exactly one span skips the row*k mul.lo; zero column offset mov.u64 %rd12, 0). Negative regressions kept/added: illegal 3x8-into-4x8 shape (kept), and a NEW hand-built ADD(col, x) fixture proving a broadcast input 0 throws E-LRED-BCAST.

REACHABILITY (verified 2026-07-08): cad.f SHP-LEGAL? admits only BIAS 1xC, SCALE 1x1/same, and ADD/MUL/RESIDUAL-ADD same-shape; RELU/GELU/SILU are unary (no param operand; PLAN-SHP-NODE skips <2-input EW nodes). The post-capture IR re-check (cad.f RB-* SHP-CHECK) enforces the same classes over the built IR. LRED-EW-OP? admits no op outside that set, so NO captured model can put an Rx1 operand into a row-reduce region; both the positive COL fixture and the input-0 negative are hand-built MIR (backward-test pattern), and the BC-COL lowering is defense-in-depth cover. SHP-LEGAL? was NOT extended (out of scope here).

Gates 2026-07-08 (macOS host): bin/hb --load maki/lower-red-test.f -> test: ok; bin/hb --load maki/test.f -> 73 PASS, test: ok; typed-local-diff-lint clean; dot-dep-lint 0 findings. Note: the focused runner-entry slice `bin/hb --load test/gate-runner-support.f test/gate-runner-entry.f -- lint-libs-ptx` exits 77 on the PRISTINE tree too (pre-existing host/harness issue, dotted as habu-focused-gate-runner-12b9812a); lib/ptx tests pass via direct focused loads (lib/ptx/collective-test.f, lib/ptx/tile-test.f -> test: ok).

PENDING-ZED:
- Device golden for the Rx1 fold once the Orin is reachable: drive the hand-built ADD(4x1 col, 4x8) -> RMSNORM region through the lower device path (maki/lower-mv-device-test.f pattern: LOWER-DRIVER! child emit -> ptxas ASSEMBLE -> LRED-RUN vs host executor under the reduction tolerance). The child driver needs the hand-built MIR text (no MODEL: capture exists for an Rx1 into RED), so extend the driver writer usage accordingly. Close the dot only after the device golden passes on the Orin.
