---
title: "Drop the type family's own member checks"
status: closed
priority: 2
issue-type: task
created-at: "2026-11-22T15:08:02.386587+01:00"
closed-at: "2026-11-23T10:11:21.840825+01:00"
---

Problem: checks the census (/tmp/jerry-play/census.md) traced to the type layer's own writes still run on every declaration (root CLAUDE.md: no runtime checks). Acceptance: the rows /tmp/jerry-play/brief-p3.md names are gone; the engine builds; enum-decl, structure-decl, type-decl and c2-records-source pass. Depends: none. Claim: agent=jerry workspace=.jj-ws.noindex/jerry-land.
