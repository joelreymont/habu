---
title: Read SPARQL emit strings as str
status: open
priority: 2
issue-type: task
created-at: "2026-11-24T21:32:20.197111+01:00"
blocks:
  - habu-compile-sparql-prop-cbd802a3
  - habu-compile-sparql-bind-8fde2c73
---

Problem: emit.f and emit-expression.f start with split-str. Acceptance: no split-str or ^u8 n in them; sparql-test.f passes. Depends: the paths and BIND/VALUES dots. Claim: unassigned.
