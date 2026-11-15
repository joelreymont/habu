---
title: "Name a polymorphic local's type as such"
status: closed
priority: 2
issue-type: task
created-at: "2026-11-15T21:49:48.952605+01:00"
closed-at: "2026-11-16T04:07:35.359193+01:00"
---

Problem: src/core/checker.f LOC-TYPED rejects a scheme type through BAD-LOC-ANN, whose BAD-SIG-TYPE sets the unknown-type verdict, so the record says unknown type; src/core/check-walk.f ITEM-IN-GROUP (:719) reaches LOC-TYPED for a define-word LOCAL item without SGBAD-LOCAL, so the record names the item as a signature. Acceptance: each path a program can reach gives a record saying the local cannot hold a polymorphic type, naming the local. Depends: none. Claim: agent=jerry workspace=.jj-ws.noindex/jerry-c4m2 (in the integration stack, closes at landing).
