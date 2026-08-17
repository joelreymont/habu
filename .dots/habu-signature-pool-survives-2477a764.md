---
title: Signature pool survives rollback pops
status: open
priority: 2
issue-type: task
created-at: "2026-08-17T15:30:42.484364+02:00"
---

Audit major (2026-08-17): RBF-POP rewinds SYM-N and retires the sym-keyed hash index (checker.f:12631 HIDX-SYMS-RETIRE - the tree KNOWS sym rewind invalidates sym-keyed caches) but no ASIG mark is in the frame: ASIG-ROW-U/ASIG-STR-U/ASIG-LAST (sym-keyed, :4984) keep rows written inside a popped scope. Reached today: every SUMTYPE/STRUCTURE/ENUM in an armed window runs TDPLAN-PREFLIGHT-CHECKER inside a candidate frame, captures rows, pops; the re-interned real definitions reuse rewound sym ids. Deterministic realignment + the byte-identical dedup skip makes it CORRECT BY LUCK; a misalignment makes ASIG-LAST@ answer a DIFFERENT word's row, silently defeating CHECKER-ASIG-MISSING? - the audit built to fail loudly. Fix shape (mirrors existing code): retire ASIG-LAST entries at/above the restored SYM-N in RBF-POP, or carry ASIG marks in the frame while armed. Regression: a preflight-then-misalign forge proving the stale row is unreachable; mutation = drop the retire, red by name.
