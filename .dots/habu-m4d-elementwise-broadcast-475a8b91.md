---
title: "M4d: elementwise + broadcast ops typed"
status: open
priority: 2
issue-type: task
created-at: "2026-06-27T00:11:55.484922+02:00"
blocks:
  - habu-m4c-load-store-54e8bf9c
---

Decomposes M4. Define SCALE ( tile uniform -- tile ), +. -. *. /. ( tile tile -- tile, mask must match ), B- B/ ( tile uniform -- tile ), FMA. ( uniform tile tile -- tile ). T-generic but lower to float (Resolved-M1/M2 #6: add an int-vs-float capability constraint). Mixing two different mask tokens rejects.
- Files: lib/ptx-tile.f.
- Verify: +. with mismatched masks rejects; SCALE threads the mask; two-rounding SCALE-then-+. vs one-rounding FMA. distinction documented (codegen is M4e/later).
- Dep: M4c.
