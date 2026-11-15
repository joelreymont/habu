---
title: One view of the loader cells and one place rule
status: open
priority: 2
issue-type: task
created-at: "2026-11-14T17:43:49.887720+01:00"
---

Problem: the loader cells have several views: SRCLOC:OWNED-CELL in src/core/rejection.f OWNED, src/core/checker.f SRC-OWNED and src/core/check-walk.f SRCLOC FILE-START; INB-CELL in rejection.f INPUT-START and src/core/check-hook.f; INE-CELL in rejection.f INPUT-END, check-hook.f and src/core/include.f. checker.f CHECK:LINE-AT and TOP-SITE place a top-level token by a second rule beside REJECTION's (rejection.f PLACE). Acceptance: each cell has one view, in rejection.f; LINE-AT and TOP-SITE are deleted, their last callers (render.f DSITE, DLOC-FIELDS, CAST-ERROR SITE) placing through REJECTION:AT. Depends: habu-report-a-def-2b4ff9c9 habu-report-checker-record-15362e5c habu-report-the-interpreter-10ba8b10. Claim: unassigned.
