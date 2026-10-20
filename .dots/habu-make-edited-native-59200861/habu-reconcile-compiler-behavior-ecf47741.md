---
title: Reconcile compiler behavior regressions
status: closed
priority: 2
issue-type: task
created-at: "2026-10-20T13:16:56.191108+03:00"
closed-at: "2026-10-20T18:19:57.657033+03:00"
close-reason: "Integrated reviewed implementation passes the final native registry: 685/685, exit 0, on a30a9b64 engine and 7557c59b map. Existing engine early/middle/late edit E2E passes cached/cold executable and map equality, runtime and rejection checks; five c5 generations match, and the final master-reconciled a30 product matches its ordinary self-build with cold-prefix boot passing. Final native log: /Users/joel/.cache/tmp/carl-observed-runtime.THrB7k/final-a30/native.stdout. This closes the implemented correctness fix; it does not claim subsecond edits, a five-minute suite, or unfinished optional optimizations."
---

HIR and fetch-snapshot pass after their fixtures account for constant DATA-origin metadata. The stripped primitive tick selects its scalar effect at a private TRUSTED boundary; its subject and full stripped-image row pass (165372 bytes). Hookless-reject passes with the ordinary CLI's failed-maker message checked on stderr; all probe programs and meaningful refusals remain intact. FM-PAIR's measured add/sub cancellation was blocked by tail-call link-frame restoration; the emitter now carries pending data-stack moves through LINKLOAD/RELEASE alone, preserving the unchanged codegen regression. Baked validation needs a matching product: the supplied host refuses source checking with E-ENGINE-PROVIDED. Root owns matching-product native/convergence gates, independent review and completion.
