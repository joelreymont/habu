---
title: "TFAM 9: construct + MATCH token protocol + checking"
status: open
priority: 2
issue-type: task
created-at: "2026-07-03T23:36:48.945599+02:00"
---

PLAN.md item 9. FIRST migrate lib/task.f CONSTRUCT (210, 430, call sites) to non-colliding name (case-folded lookup). Then reserve construct/MATCH/ENDMATCH + branch tokens; construct family variant resolves (owning-package-id, family-id, variant-id) in owning package; match-mode token capture before dictionary lookup; growable CF-MATCH frames (family id, type args, base rows, seen variants, branch rows, dead paths, span) with fail-closed overflow; exhaustiveness (no default branch in v1); linear-payload matches reject until TFAM 11; CASE fixtures stay green. Gate 17j. Depends: TFAM 7, 8, 12.
