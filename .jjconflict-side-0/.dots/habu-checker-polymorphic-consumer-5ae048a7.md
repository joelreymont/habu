---
title: "Checker: polymorphic consumer over a parameterized sum (result<a,b>) is not expressible"
status: open
priority: 2
issue-type: task
created-at: "2026-07-13T20:14:01.128862+02:00"
---

Problem: a word typed ( result<a,b> -- ) that MATCHes the sum cannot be defined - MATCH on a sum whose payload widths are type variables emits a runtime family lookup by name ('undefined word result'), and a plain drop rejects ('expected: a actual: result<b,c>') because the polymorphic sum's width is unknown. A CONCRETE result<X,Y> both MATCHes and drops fine. This blocks a generic RESULT-DROP / RESULT-MAP over an arbitrary result, which the R7 typestate skeleton (maki/typestate.f, dot a0eb43a2) wanted for its transition returns - it worked around it by returning the next stage DIRECTLY instead of result<stage,diag-set>. Fix: teach the checker/runtime to consume a parameterized sum polymorphically (uniform-width representation or a width-erased MATCH), so a generic result/option consumer is expressible. Acceptance: ( result<a,b> -- ) MATCH result ok OF drop ENDOF err OF drop ENDOF ;MATCH certifies AND runs; a generic RESULT-MAP typechecks. Files: src/core/checker.f (MATCH width handling), src/core/sumtype.f, a negative+positive fixture. Verify: fixtures, test/run.f. Depends: none. Ownership: checker parameterized-sum consumption. Claim: unassigned.
