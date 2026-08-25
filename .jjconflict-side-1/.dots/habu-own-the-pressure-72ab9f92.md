---
title: "Own the pressure residue's three rows"
status: open
priority: 2
issue-type: task
created-at: "2026-08-14T04:23:19.773245+02:00"
---

Found by the singletons diagnosis (95b79b3a): SHA-PAD (E-A64RA-POOL, regalloc.f:2075 MB-SPARE-N), FIELD-REG (E-A64RA-SPILL, :2076 - pool exhaustion one position earlier, proven by opening MB-SPILLABLE?), SPLIT-NEXT (E-A64RA-SPILL, genuinely dominance-bound - dropping KEEP? moves it to E-IR-VERIFY-DOM, so the guard is load-bearing and relaxing it would miscompile). Every prior owner closed; the MB-KEEP-BLOCK refutation re-verified against these exact rows (opening both clauses moves nothing). Whoever takes this starts from those attributions; the fix is capacity/placement work, not guard relaxation. Files: src/compiler/native/{regalloc,spill}.f. Depends: none.
