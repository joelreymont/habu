---
title: Give the IR aggregate and tagged-union kinds
status: open
priority: 2
issue-type: task
created-at: "2026-08-06T16:07:09.748878+02:00"
---

The chain's IR has 8 flat kinds and no struct, array, or tagged-union representation (thecut audit) — ADT match/construct and STRUCTURE values have nothing to compile into, so the whole ADT-using stdlib is outside the dialect. Design the aggregate substrate on the typed frozen IR (kinds, layout derivation from the checker's own layout facts, verify.f rules), sized by what the stdlib actually uses (measure first: count the ADT/structure shapes in chain-target files). Blocks the dialect-completion tranches and habu-cut-colon-compilation-a5aa3f1f.
