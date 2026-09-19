---
title: Keep FEXP finite at the overflow threshold
status: open
priority: 3
issue-type: task
created-at: "2026-09-20T00:13:38.382401+03:00"
---

Problem (audit of f468d914): lib/fmath.f:230-235 FEXP compares x 709.782712893384 f> strictly, so x = 709.782712893384 (ln DBL_MAX) falls through: k = 1024, the degree-6 polynomial answers about 1.0 and LDEXP-STEPS overflows; 709.782712893384 FMATH:FEXP IEEE754:F64>BITS = 0x7FF0000000000000 (+inf) where glibc answers 0x7FEFFFFFFFFFFFFF. 709.78271289338 is finite and the underflow side (-745.1332191019411 -> bits 1) matches glibc. Acceptance: FEXP at the exact threshold answers the finite value (reduce with k = 1023 near the top, or scale the last step against the polynomial's actual magnitude), and a fixture pins the threshold, the value just above it (+inf) and just below it; lib/fmath-test.f unchanged in what else it asserts. Files: lib/fmath.f, lib/fmath-test.f, docs/fmath.md. Verify: bin/hb --load lib/fmath-test.f. Depends: none. Ownership: lib (alder). Claim: unassigned.
