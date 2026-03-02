---
title: Audit missing upstream modules against maxima.system
status: closed
priority: 1
issue-type: task
created-at: "\"2026-03-07T19:20:07.539876+01:00\""
closed-at: "2026-03-07T22:32:53.042979+01:00"
close-reason: "done (audited maxima.system against lib/maxima-loader.lisp and promoted cleanly-loading upstream modules in order: mtrace, mdebug, polynomialp, desoln, elim, invert, hypgeo, hyp, hypergeometric, nfloat; validated loader completeness at OK=157 FAIL=0 ATTEMPTED=157)"
---

lib/maxima-loader.lisp:27-86 and ../maxima/src/maxima.system:566-779. Root cause: the eager-load list predates review findings and omits multiple modules with testsuite impact. Fix: classify modules into eager-load now / autoload / deferred / blocked. Why: turns Stage-1/2 module work into a precise closure list.
