---
title: Keep unit section cursor local
status: closed
priority: 1
issue-type: task
created-at: "2026-10-19T14:28:25.959597+03:00"
closed-at: "2026-10-20T18:19:58.156301+03:00"
close-reason: "Integrated reviewed implementation passes the final native registry: 685/685, exit 0, on a30a9b64 engine and 7557c59b map. Existing engine early/middle/late edit E2E passes cached/cold executable and map equality, runtime and rejection checks; five c5 generations match, and the final master-reconciled a30 product matches its ordinary self-build with cold-prefix boot passing. Final native log: /Users/joel/.cache/tmp/carl-observed-runtime.THrB7k/final-a30/native.stdout. This closes the implemented correctness fix; it does not claim subsecond edits, a five-minute suite, or unfinished optional optimizations."
---

Existing body-incomplete cache E2E only fails final whole-image equality on21b4. Exact logical difference is NUNIT-FILE:SECTION-OFF5698warm versus5843fresh; public section readers use independent A/U tables. READ-SECTIONS/WRITE-SECTIONS each initialize this private traversal cursor. Carry offsets within each invocation and remove global without changing format/version, section tables, bounds/overflow validation, empty-tail support, refusal codes or failure order. Existing body-incomplete equality is pre-code failure; existing unit codec malformed/roundtrip and E2E checks plus independent review, final matching native/fullsuite/convergence. No added cleanup reset or weak equality.
