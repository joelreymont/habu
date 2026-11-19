---
title: "Check a quotation's tail escape once per binding"
status: open
priority: 2
issue-type: task
created-at: "2026-11-19T15:17:02.800531+01:00"
---

Problem: HOLD-ESCAPE? (src/core/checker.f, run by QUOT-PAIR after each comparison) walks the trail from its frame's mark and, for each var older than the frame, walks its binding for the frame's rigid tails; an enclosing frame's range holds every inner frame's entries, so k nested comparisons check k(k+1)/2 entries (5050 unowned entries at k=100, 20100 at 200: 13-18% of QUOT-CALL at k=2047). The (frame, entry) pairs differ for real, so no local index removes it. Acceptance: an escape (a var older than a frame bound, in the drained graph, to a term holding that frame's rigid tail) is found with work linear in the bindings, giving the same verdicts as today on test/run.f checker-soundness, signature-nesting and native-quote-forward, and QUOT-CALL's HOLD-ESCAPE? share stays flat from k=400 to 2047. Depends: none. Claim: unassigned.
