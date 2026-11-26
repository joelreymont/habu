---
title: "Write SPARQL's SQL from a statement tree"
status: open
priority: 2
issue-type: task
created-at: "2026-11-26T19:41:18.701042+01:00"
---

Problem: emit.f writes SQL by copying text at every nesting level, which costs the compile's memory and blocks rewriting a statement before it is written (Joel 2026-11-26: "you should be lexing, parsing and building a IR here, not copying text"). Acceptance: the SQL is a tree written out once; every SPARQL test's statement and the measured shapes' byte-identical to before. Depends: none.
