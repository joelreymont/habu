---
title: Write a SPARQL walk read more than once once
status: open
priority: 2
issue-type: task
created-at: "2026-11-27T20:32:37.431297+01:00"
---

Problem: src/lib/sparql/emit.f WALK-CTE writes each walk's seed and step CTEs in a WITH RECURSIVE on the walk's own relation, so a walk that the query reads several times is computed each time: Tender's survey (/tmp/carl-sparql/survey/survey-path.rq) reads ?a zen:has+ ?L, ?a zen:has+ ?m and ?m zen:has+ ?L in graph urn:zen:offer, three textually equal recursive CTEs of about 46 ms each per evaluation of their branch. The same holds for an undriven union read twice (s-union-250's 21 union texts are byte-equal). Acceptance: a walk or undriven union whose text, its own names aside, equals an earlier one's in the statement, and which reads no outer binding, is written once and read by every use; the survey's three walks run once; answers unchanged on sparql-test.f and w3c-test.f. Depends: none. Claim: unassigned.
