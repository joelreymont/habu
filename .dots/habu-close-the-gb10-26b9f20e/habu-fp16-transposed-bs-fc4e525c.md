---
title: fp16 transposed-Bs feed
status: closed
priority: 1
issue-type: task
created-at: "\"2026-07-19T12:12:03.463453+02:00\""
closed-at: "2026-07-19T13:01:01.916731+02:00"
---

The fp16 residual lever (from the fp16 tile closure, campaign habu-close-the-gb10-26b9f20e): the fp16 B fragment is built from two ld.shared.u16 + shift/or per b32 register because Bs is k-major, so per-K feed instruction count stays at tf32 levels while mma count halved - fp16 reaches 1.45-1.58x tf32 instead of the ~2x the shape offers. Store Bs TRANSPOSED for fp16 (n-major, K-adjacent halves contiguous) so each B register is ONE ld.shared.b32, mirroring the tf32 B-ldmatrix wave: new staging layout + cp.async chunk mapping, fragment loader drops the shift/or pair. Element-exact first (extend MGC-CFG-F16 rows), tf32 byte-identity preserved, then the doc timing protocol; predict the 512-2048 fp16 numbers move toward Triton's 27/74/86. GPU timing solo.
