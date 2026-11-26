---
title: Compile a SPARQL query in little memory
status: open
priority: 2
issue-type: task
created-at: "2026-11-26T18:43:18.833397+01:00"
---

Problem: a SPARQL compile of a few hundred bytes takes 16.5 MB RSS, a nest of walks seeded by the solutions so far 24 to 50 MB at depth 16 to 39, and path-mixed 26 and 47 MB at 7 and 14 elements while its SQL grows linearly (Joel 2026-11-26: "this smells! why so much memory needed? rca and troubleshoot!"). Joel 2026-11-26: "we should be using 1mb max here". Acceptance: the compile's memory above the engine's own is at most 1 MB for every query of the SPARQL tests and the measured shapes, each share attributed to the words holding it. Depends: none.
