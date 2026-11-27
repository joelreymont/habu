---
title: Name a SPARQL call without copying it
status: closed
priority: 2
issue-type: task
created-at: "2026-11-27T21:04:10.845328+01:00"
closed-at: "2026-11-27T22:36:33.258819+01:00"
---

Problem: src/lib/sparql/expression.f READ-CALL copies every builtin call's name into the syntax arena to upper-case it. Joel: lex and parse into an IR, not copying text. Acceptance: a call's name costs no copy; every statement the SPARQL tests compile is unchanged. Depends: none.
