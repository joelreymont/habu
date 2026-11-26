---
title: Share one CTE per equal SPARQL walk step
status: open
priority: 2
issue-type: task
created-at: "2026-11-26T23:27:29.346716+01:00"
---

Problem: two walks over an equal operand, direction and graph each write their own statement CTE, and each walk builds its own node test, so PostgreSQL reads the same rows twice (design /tmp/carl-sparql/design-tree.md, SHARE). Acceptance: such walks read one CTE, and one node test is written per graph and key form; every SPARQL test answers as before; a two-walk query's statement has one CTE where it had two. Depends: habu-write-sparql-s-9021d6f9.
