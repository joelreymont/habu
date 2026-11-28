---
title: Infer SPARQL shapes into nested groups
status: closed
priority: 2
issue-type: task
created-at: "2026-11-26T23:54:52.697950+01:00"
closed-at: "2026-11-28T05:38:19.412207+01:00"
---

Problem: a nested group (OPTIONAL, MINUS, a UNION branch) is compiled alone (emit.f OPTIONAL-ITEM, MINUS-ITEM, GROUP), so shapes and constants its enclosing group already fixes do not narrow its choices. Example: ?f rdf:predicate P . OPTIONAL { ?f rdf:object ?w } keeps every value column of ?w and stays generic, where the same two triples in one group bind ?w to P's column. Acceptance: what the enclosing group's earlier joined items fix narrows the choices of a nested OPTIONAL, MINUS or UNION branch one level down, never into EXISTS; that example binds ?w natively with its answers unchanged, and a test shows it. Depends: none.
