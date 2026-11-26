---
title: Fold SPARQL conditions known at compile time
status: open
priority: 2
issue-type: task
created-at: "2026-11-26T23:27:29.321492+01:00"
---

Problem: the compiled statement keeps conditions the compiler already knows: a boolean identity with a static operand, a comparison of two known untyped terms, a WHERE that is statically false, a duplicate conjunct, and in a walk's node test a disjunct another EXISTS over the same source implies. They cost statement bytes, compile memory and PostgreSQL's planning (design /tmp/carl-sparql/design-tree.md, FOLD). Acceptance: each listed condition is folded where the tree is built; every SPARQL test answers as before; no measured shape's statement grows. Depends: habu-write-sparql-s-9021d6f9.
