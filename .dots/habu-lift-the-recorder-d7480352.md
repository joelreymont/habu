---
title: Lift the recorder caps and re-census past them
status: open
priority: 2
issue-type: task
created-at: "2026-08-12T20:50:32.050063+02:00"
---

Census 2026-08-12: 151 definitions are UNMEASURED (not proven-compilable) behind E-NMIGRATE-TEXT, the 512-byte source recorder cap (date.f DAYS>YMD 851B; lib/ptx/cg-mma.f alone has 41). The cut needs these measured: lift the cap (or page the recorder) and re-census the 151. Files: src/compiler/native/migrate.f (recorder). Depends: none.
