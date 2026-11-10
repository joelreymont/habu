---
title: A typed-storage definer into a sealed package is refused under a generated name and no line
status: open
priority: 2
issue-type: task
created-at: "2026-11-10T15:58:46.545241+02:00"
---

Problem: with the seal refusal of habu-a-load-accepts-db95cbfb, a typed-storage definer (typed variable, typed buffer, dynamic buffer, heap, arena, layout buffer, STRUCTURE with OPAQUE) whose name goes into a sealed package is refused under the load with the definer's generated name and no line, where the source declared one token at one line. The definers' name checks (src/core/layout-buffer.f LBUF-NAME-OK?, heap.f, arena.f) load before everything the seal test reads (layout.f, xref.f, packages.f, definers.f, outer.f SEALED-FAIL, check-hook.f AT-SOURCE). Twelve probes: /Users/joel/Work/reviews/habu-10x-2026-11-10/cases8/seal-review p/in-tvar.f out-tvar.f in-tbuf.f in-dbuf.f in-heap.f in-arena.f in-lbuf.f out-lbuf.f out-dbuf.f out-heap.f out-tbuf.f and q/in-opaque.f. Found by the review of the seal commit (finding 2); the fold-in worker's stop report is briefs8/reports/seal-fold2-stop.md. Source: Joel: "i want all issues identified to be dotted and fixed". Hunt first: why a refusal inside a definer's evaluated text carries no line, and how the refusal names the declared token with one seal test. Acceptance: each probe is refused E-SEALED-PACKAGE at the declared token and its line, the same under the load and the check. Files: named by the hunt. Verify: the twelve probes. Depends: habu-a-load-accepts-db95cbfb commit one landed. Ownership: the seal refusal. Claim: unassigned.

Settled 2026-11-10 by the long-term rule, two commits after seal commit one lands. One: a public guard word in src/habu/definers.f that each typed-storage definer's generated text calls first with the declared name, so the refusal names the declared token. Two: a refusal inside evaluated text reports the place of the outer statement in the file; this reverses test/load-reject-diag-test.f TEST-EVALUATED-PLACE and the rule at checker.f CHECK:LINE-AT. Its brief starts by establishing whether habu2.f LSRCAT is reached under --load; if it is, that part joins the interpreter's replacement.
