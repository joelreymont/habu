---
title: Move three reference blocks out of LESSONS
status: open
priority: 2
issue-type: task
created-at: "2026-08-17T19:58:08.895630+02:00"
---

Flagged by lessons-close (2026-08-17): three consolidated LESSONS entries are half reference material - the lldb recipe belongs in docs/debugging.md (the LESSON - seeded vs ordinary is the split - stays), the TF-CTOR-ESC naming table belongs in docs/forth.md Packages, the stale-binary recovery procedure belongs in docs/bootstrap.md. Move the reference halves, leave the lessons with cross-references. Also on the same leaf: ~12 smaller duplicate groups remain itemized on 16f2510d's leaf (registration-is-not-execution ~4, using-shadow family ~5, arena-slot exhaustion ~5, typed-local added-lines 3, local-shadows 4, jj-destructive-target ~5, one-fact-one-word ~5, live-bin-hb 2) - finish them with the token-enumeration no-loss method the lane proved.
