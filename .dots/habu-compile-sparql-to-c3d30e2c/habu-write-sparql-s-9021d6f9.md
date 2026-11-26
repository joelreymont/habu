---
title: "Write SPARQL's SQL from a statement tree"
status: open
priority: 2
issue-type: task
created-at: "2026-11-26T19:41:18.701042+01:00"
---

Problem: a SPARQL binding's value and an aggregate's are strings, so every read copies them, and an [NOT] EXISTS inside a BIND, GROUP BY, ORDER BY or aggregate is written out and copied again at each nesting level (Joel 2026-11-26: "you should be lexing, parsing and building a IR here, not copying text"). Acceptance: binding and aggregate values are rels added by reference; every SPARQL test's statement and the measured shapes' byte-identical to before, except where an exact copy count moves a BIND between a copied column and an evaluated table. Depends: none.
