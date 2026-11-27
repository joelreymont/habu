---
title: "Order a SPARQL GROUP_CONCAT's values"
status: open
priority: 2
issue-type: task
created-at: "2026-11-27T09:51:30.507935+01:00"
---

Problem: SPARQL 1.1 leaves GROUP_CONCAT's order undefined, so a query cannot join a path's labels top down (Tender's goods survey, its context query in Zen). Joel 2026-11-27 ruled a Habu extension: GROUP_CONCAT(?label; SEPARATOR=" / "; ORDER BY DESC(?depth)), compiled to an ordered aggregate. Acceptance: the values join in the ORDER BY conditions' order (ASC, DESC, several keys, DISTINCT keeping each value's first place); ORDER BY elsewhere in a call, empty, or before SEPARATOR is rejected with its fix; docs/sparql.md states the extension. Depends: none. Claim: unassigned.
