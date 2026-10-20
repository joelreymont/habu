---
title: Fix automatic FILE gate failures
status: closed
priority: 2
issue-type: task
created-at: "2026-10-20T13:16:56.168940+03:00"
closed-at: "2026-10-20T18:19:57.381678+03:00"
close-reason: "Integrated reviewed implementation passes the final native registry: 685/685, exit 0, on a30a9b64 engine and 7557c59b map. Existing engine early/middle/late edit E2E passes cached/cold executable and map equality, runtime and rejection checks; five c5 generations match, and the final master-reconciled a30 product matches its ordinary self-build with cold-prefix boot passing. Final native log: /Users/joel/.cache/tmp/carl-observed-runtime.THrB7k/final-a30/native.stdout. This closes the implemented correctness fix; it does not claim subsecond edits, a five-minute suite, or unfinished optional optimizations."
---

Automatic definition collection subscribed to resolution rows without enabling their source checker producer. The base cold shadow refused with rc 70 while its warm body skip succeeded with binding 41. Pair the producer with the definition dependency lifetime and invalidate incomplete reader/definition keys. Focused using-scope and primitive-shadow cases, full replay E2E and full auto E2E pass against the supplied sealed engine and live tools; auto uses the root-provided whitebox bundle. Warm shadow now refuses with rc 70, and an unchanged consumer still hits/skips once with binding 41. Both original expectations stay. Root integration, combined verification and independent review remain required. Logs and repeatable artifacts are under /Users/joel/.cache/tmp/carl-auto-cache-probe; keep this dot active for the integrator.
