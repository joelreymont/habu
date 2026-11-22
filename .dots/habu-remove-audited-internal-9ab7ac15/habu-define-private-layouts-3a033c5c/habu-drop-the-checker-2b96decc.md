---
title: "Drop the checker's own layout checks"
status: active
priority: 2
issue-type: task
created-at: "2026-11-22T15:08:02.414909+01:00"
---

Problem: checks the census (/tmp/jerry-play/census.md) traced to the type layer's own writes still run on every declaration (root CLAUDE.md: no runtime checks). Acceptance: the rows /tmp/jerry-play/brief-p4.md names are gone; the engine builds; enum-decl, structure-decl, type-decl and c2-records-source pass. Depends: none. Claim: agent=worker workspace=.jj-ws.noindex/jerry-p4.
