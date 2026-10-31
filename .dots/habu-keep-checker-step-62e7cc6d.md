---
title: Keep checker step state out of captured images
status: open
priority: 2
issue-type: task
created-at: "2026-10-31T11:47:17.223076+02:00"
---

Problem: a checker cell a checked step writes and nothing resets at a file boundary holds what the last checked step left, so an image captured after a warm build (reused files are not checked) can differ from a fresh build's. Lane 11 measured it (LMNEG, types e2e 46/51; fixed with LMPOS, LMV, LMI, LINBEF, LINEXP, LTNT-I by stack values). A read-only census of src/core/checker.f at utnlqywz (/tmp/carl-census2/report.md, brief carl-dots/.out/carl-file/brief-checker-census.md) finds the class broad: one-word scratch (TCMP, CT-J, FM-CUR, NRES, CK-AOT-*, DOS-*, ...), per-definition counters reset in NEW outside a span (RIGID-N, RGN-N, EXT-N, GEN-N), unify cursors reset only when UNIFY starts (UOK, UF-*, CUR-*), SCOPE-N never reset, and cells reset only at capture. Acceptance: every checker cell meaningful only during checking is a local or stack value or lives in a CHECKER-REG:SCRATCH span (zeroed at each file boundary and at capture); warm image == fresh-cache image on the lane 11 cases. Files: src/core/checker.f. Verify: engine rebuild, types and frame e2e, the m22 pair, full suite (shared checker behaviour). Depends: lane 11 landed (same file). Claim: unassigned.
