---
title: Keep catch return state local
status: closed
priority: 1
issue-type: task
created-at: "2026-10-19T14:21:24.176664+03:00"
closed-at: "2026-10-20T18:19:58.590369+03:00"
close-reason: "Integrated reviewed implementation passes the final native registry: 685/685, exit 0, on a30a9b64 engine and 7557c59b map. Existing engine early/middle/late edit E2E passes cached/cold executable and map equality, runtime and rejection checks; five c5 generations match, and the final master-reconciled a30 product matches its ordinary self-build with cold-prefix boot passing. Final native log: /Users/joel/.cache/tmp/carl-observed-runtime.THrB7k/final-a30/native.stdout. This closes the implemented correctness fix; it does not claim subsecond edits, a five-minute suite, or unfinished optional optimizations."
---

Exact decoded same-source native hb2 versus hb3 differs only at checker RSRET DATA offset990352 (-1 to0); all26697effect/control records, AOTcode and map rows are identical. RSRET is written before every read and belongs solely to one RSCATCH invocation. Replace persistent scratch with an immutable returns local after guarded quotation instantiation, preserving post-unification DEADP timing, stale-row handling, opaque/nonquote refusals and exception semantics. No signature normalization, new helper or capture reset registry. Existing generation byte inequality is the pre-code failure; use existing native catch/catch-stale E2Es, exact native products, fullnative/convergence and independent Astra review.
