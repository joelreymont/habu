---
title: "Reject SPARQL BOUND's argument over its text"
status: closed
priority: 2
issue-type: task
created-at: "2026-11-27T21:04:10.830201+01:00"
closed-at: "2026-11-27T22:36:33.233977+01:00"
---

Problem: src/lib/sparql/expression.f BOUND's argument rejection is placed at the argument node's token; terms are shared, so SELECT * WHERE { ?s ?p 1 FILTER(BOUND(1)) } reports 1:24, the triple's object, and its fix rewrites that triple; BOUND("abc") and BOUND(?s + 1) report the ) after the argument and following the fix cycles. Joel: a rejection's fix, applied, fixes it. Acceptance: each is placed at the argument and the test follower reaches an accepted query. Depends: none.
