---
title: Keep pre-window created words as calls in a T0 capture
status: open
priority: 1
issue-type: task
created-at: "2026-10-29T05:07:29.202127+02:00"
---

A6 prerequisite. T0 folds every ADDR word (created word, variable) into a body as its address. The JIT keeps a created word defined before an open AOT capture window as a call (habu2.f:12988-13002, aot-arm.f:14-20, layout.f:2392, aot-capture.f ACAP-OUT-CHAIN), because its address is not one the captured image relocates. Under T0 as the default compiler inside a capture window, a folded pre-window address would be baked as a host address. Match the JIT: compile pre-window ADDR words as calls, or refuse the capture. Source: B lane review (jerry-t0b mpnwmrqu).
