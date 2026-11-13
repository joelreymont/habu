---
title: "Keep a stripped program's data by extent"
status: active
priority: 2
issue-type: task
created-at: "2026-11-13T23:28:05.945462+01:00"
---

Problem: src/habu/link-copy.f's unit of data liveness is a whole file (UNIT-OF, CUT-DEAD), so one live address cell keeps every byte its file laid. Acceptance: the unit is the build table's extent; a header cell a piece stored is read once one of its extents is live or one of its records' emissions is placed; an engine link is byte-equal to today's; the reproducer's DATA shrinks and it prints the same. Depends: none. Claim: agent=jerry workspace=.jj-ws.noindex/jerry-ext.
