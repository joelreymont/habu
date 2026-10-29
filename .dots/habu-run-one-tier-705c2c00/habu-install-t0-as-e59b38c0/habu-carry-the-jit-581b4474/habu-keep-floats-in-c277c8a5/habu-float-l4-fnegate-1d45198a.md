---
title: "Float L4: fnegate fabs fsqrt compares and conversions in T0"
status: open
priority: 2
issue-type: task
created-at: "2026-10-29T11:33:45.553652+02:00"
blocks:
  - habu-float-l3-f-549d53b1
---

FUNARY/FUN, FCMP, CC-FEQ..CC-FNGT (arm64 EQ NE MI PL GT LE), INT>FLOAT, FLOAT>INT; t0.f FUN1, FCMP2, CVT-SF, CVT-FS; f0< f0= as 0.0 + FCMP2 (no FCMP0). Runtime rows 8-11, encoding row 3.
