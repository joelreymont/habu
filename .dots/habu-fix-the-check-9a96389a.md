---
title: "Fix the CHECK! probing idiom's stale name"
status: open
priority: 2
issue-type: task
created-at: "2026-08-14T05:46:50.941899+02:00"
---

Found by the create-axiom lane: the documented top-level probing idiom s-quote-text CHECK! dies rc 78 E-DUP-DEFINITION after a few certifying probes - the name NM is stale engine state, so a certifying probe records its signature under the PREVIOUS definition's name and the next collides; diagnostics also mis-attribute (habu: in dup: at '+'). Reproduced on base. Fix the stale-state root in the check entry (reset NM per CHECK!), regression with ten sequential probes, and correct the doc if the idiom changes. Files: src/core/checker.f (check entry), docs. Depends: none.
