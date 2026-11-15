---
title: Refuse a primitive row its facts text cannot say
status: active
priority: 2
issue-type: task
created-at: "2026-11-15T20:35:39.983889+02:00"
---

Problem: src/core/check-hook.f ROWS-TEXT? (about :620-640) answers false when a primitive row does not survive its facts text (FACTS-SAME-AT?, src/core/render.f:771), and the build then stores no facts for the primitive, so a later run reports the defined primitive as an undefined word in every definition that calls it (seen with str's >str, whose row pushed a two-cell type as one cell). Acceptance: a build stops, naming the primitive and its row, when a row with an effect fails its facts text; the cases that have no text by design keep their behaviour. Depends: none. Claim: agent=worker workspace=.jj-ws.noindex/habu-facts-refuse.
