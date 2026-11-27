---
title: Step a SPARQL walk over a term slot by its native column
status: open
priority: 2
issue-type: task
created-at: "2026-11-27T14:55:36.243785+01:00"
---

Problem: src/lib/sparql/emit.f STEPS and WALK-CTE (:3379, :3640): a walk whose steps start on a slot of several columns (a GENERIC term) and end on a native column compares p.a, a term built per row, with w.n, the end converted to a term: `?m sized:part-of+ <urn:zen:ID>` on Zen's sized base scans all 49,950 part-of quads at each of 999 steps, 18.9 s, past the query tool's 5 s. Acceptance: such a step compares the pairs' column of the end's root with the end; that query answers within 5 s in Zen's test/sparql.f; answers unchanged on sparql-test.f and w3c-test.f. Depends: habu-meet-a-sparql-0ec3c7f0.
