---
title: Make render.f private to package CHECK
status: active
priority: 2
issue-type: task
created-at: "2026-11-15T21:49:06.345500+01:00"
---

Problem: src/core/render.f defines 280 global words (RQM, REC-SIG, the *-INSTALL installers of checker defers, the REND-* and diagnostic words) that any program can call. Acceptance: every word of the file is private to package CHECK except WARN-DIAGS and the constants other files name; engine callers EXPORT what they call; the engine builds. Depends: none. Claim: agent=worker workspace=.jj-ws.noindex/carl-w-render.
