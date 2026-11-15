---
title: Help a does> clause with the effect it declares
status: closed
priority: 2
issue-type: task
created-at: "2026-11-15T12:47:38.656112+01:00"
closed-at: "2026-11-16T04:07:35.935655+01:00"
---

Problem: render.f END-HELP's inferred effect for a does> clause includes the data-field ptr the CREATE stub pushes, so BADARR's help says 'or declare ( n ptr b -- n )', which no clause can declare. Acceptance: the help names an effect the clause can declare. Depends: none. Claim: agent=jerry workspace=.jj-ws.noindex/jerry-c4m2 (in the integration stack, closes at landing).
