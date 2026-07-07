---
title: "RED: fold Rx1 column-broadcast operand into row kernel"
status: open
priority: 2
issue-type: task
created-at: "2026-07-07T07:30:53.399775+02:00"
---

lower-red.f LRED-CLASSIFY-INS currently fails closed (E-LRED-BCAST) on an Rx1 (BC-COL) broadcast operand: the block-per-row kernel loads a full RxC operand as base + row*k*4 (EMIT-ROW-SPAN, stride k=C), and a broadcast 1xC/1x1 pins to row 0 (EMIT-ROW-SPAN0). An Rx1 column operand instead needs base + row*1*4 (stride-1 row span) with a zero column ctx so every lane in block r reads element r (= e/C), matching executor EX-BC@ Rx1. Not a legal capture class today (cad.f SHP-LEGAL? only allows BIAS 1xC and SCALE 1x1/same), so no model produces it; the guard is defense-in-depth. To land: add an EMIT-ROW-SPAN-STRIDE1 (or param the stride) in lib/ptx/cg-collective.f, route BC-COL through it + EMIT-ZERO-OFF in LRED-LOAD-IN, convert the lower-red-test.f COL fail-closed fixture to a positive test, and add a device golden. Covered fail-closed by the COL fixture (maki/lower-red-test.f).
