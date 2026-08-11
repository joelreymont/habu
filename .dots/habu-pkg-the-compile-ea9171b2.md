---
title: Package the compile-path die family
status: open
priority: 2
issue-type: task
created-at: "2026-08-11T12:29:00.144122+02:00"
---

Residual from the DEFER-DIAG landing (75337472): C-DIE-DOES, C-DIE-TOKEN-NL, C-PD-DIE-FULL, C-DIE-BAD-TAG, C-LBRACE-DIE, C-DEFER-DIE-TOKEN are scattered unpackaged globals in src/habu/habu2.f - the same 'global emitter surface' debt the HOLD-EMIT comment refuses to extend and DEFER-DIAG just declined to join. Package them by concern (the DEFER-DIAG two-block model; measure callers first), keeping C-DEFER-DIE-TOKEN's boot-integrity token-only contract intact. Caller cascade deliberately not started in the landing. Files: src/habu/habu2.f. Depends: none.
