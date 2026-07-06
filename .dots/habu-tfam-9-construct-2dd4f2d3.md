---
title: "TFAM 9: construct + MATCH token protocol + checking"
status: active
priority: 2
issue-type: task
created-at: "\"2026-07-03T23:36:48.945599+02:00\""
---

PLAN.md item 9. Reserve construct/MATCH/ENDMATCH + branch tokens; construct family variant resolves (owning-package-id, family-id, variant-id) in owning package; match-mode token capture before dictionary lookup; growable CF-MATCH frames (family id, type args, base rows, seen variants, branch rows, dead paths, span) with fail-closed overflow; exhaustiveness (no default branch in v1); linear-payload matches reject until TFAM 11; CASE fixtures stay green. Gate 17j. Depends: TFAM 7, 8, 12.

## Audit refresh (2026-07-06, head 1eb3b5d3)

The "FIRST migrate lib/task.f CONSTRUCT" prerequisite is done: f968f1d93bad
"Rename task CONSTRUCT ahead of construct reservation" landed; no bare CONSTRUCT
token remains repo-wide (lib/task.f now uses TASK-CONSTRUCTED*). The reservation
and MATCH protocol work above is unstarted.
