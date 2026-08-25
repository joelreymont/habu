---
title: Derive TMAX from the recorder bound
status: open
priority: 2
issue-type: task
created-at: "2026-08-14T07:53:40.242052+02:00"
---

Found by the recorder landing: elaborate.f TMAX = 256 body tokens is the next cap of exactly the shape just fixed, one file over - a straight-line body compiles at 200 tokens and refuses E-NELAB-BLOCK at 300, while the recorder now admits 8000-byte bodies (up to ~4000 tokens). Derive the elaborator's token bound from the recorder's (or from its own arena constraint, stated), two-sided fixture. Files: src/compiler/native/elaborate.f. Depends: none.
