---
title: "Drop the type schema's own-write checks"
status: closed
priority: 2
issue-type: task
created-at: "2026-11-22T15:08:02.334672+01:00"
closed-at: "2026-11-23T10:11:21.852197+01:00"
---

Problem: checks the census (/tmp/jerry-play/census.md) traced to the type layer's own writes still run on every declaration (root CLAUDE.md: no runtime checks). Acceptance: the rows /tmp/jerry-play/brief-p1.md names are gone; the engine builds; enum-decl, structure-decl, type-decl and c2-records-source pass. Depends: none. Claim: agent=worker workspace=.jj-ws.noindex/jerry-p1.
