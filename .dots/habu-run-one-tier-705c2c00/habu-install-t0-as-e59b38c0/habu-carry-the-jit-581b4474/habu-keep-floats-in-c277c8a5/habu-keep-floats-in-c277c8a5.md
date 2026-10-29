---
title: Keep floats in registers in T0 as the JIT does
status: active
priority: 2
issue-type: task
created-at: "2026-10-29T09:56:38.306358+02:00"
---

JIT runs f+ f- f* f/ s>f in its d8-d31 pool; T0 calls them. Needs a float register kind in the T0-EMIT seam.

Parked (big-picture design /tmp/jerry-big/design.md step 4): zz meets the run bar without floats (T0 12.60G vs JIT 12.75G); after T0 is baked and the JIT deleted, measure a float-heavy workload against the last JIT engine and resume only if run > 1.05x. Design /tmp/jerry-float/design.md (cut: no FCMP0). L1 trzopyok (jerry-t0f1) reviewed REVISE, tests only: delete the x86 FVREGS and arm64 count rows, add an E-T0-FILES row (/tmp/jerry-t0f1/rev/files.f), MREG in LIT/PUSH and fold FLOAT? into it, BTASK-ENTRY in the header, DBASE -> D-FIRST, two comment nits. L2 zsynwuzw (jerry-t0f2) WIP, untested; handoff jerry-t0f2/.out/handoff-float2.md.

T0 line (2026-10-29): trzopyok and zsynwuzw now sit on rsnyxvvn without mnqqvykn (which was below them before). zsynwuzw redefines OWN with one argument (OWN-IN); uonzzmsw made OWN take two (the vreg the operation consumes, or NO-ENTRY), so rebasing the float commits onto pwxnuopw must adopt the two-argument OWN.
