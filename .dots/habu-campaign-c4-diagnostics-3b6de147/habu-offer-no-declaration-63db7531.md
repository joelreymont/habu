---
title: Offer no declaration with an unsaid type
status: closed
priority: 2
issue-type: task
created-at: "2026-11-15T17:25:09.112565+01:00"
closed-at: "2026-11-16T04:07:35.245528+01:00"
---

Problem: src/core/render.f END-HELP still offers 'or declare ( ... )' when the effect's text holds '?' (RQM set, e.g. RATOM-CHAR at render.f:97 past 26 atoms), which no signature spells. Acceptance: the help leaves the clause out then, pinned by an engine test. Depends: none. Claim: agent=jerry workspace=.jj-ws.noindex/jerry-c4m2 (in the integration stack, closes at landing).
