---
title: Drive SPARQL source unions by their bindings
status: active
priority: 2
issue-type: task
created-at: "2026-11-25T10:36:35.588190+01:00"
---

Problem: on Zen's sized fixture ?m sized:part-of X . ?f rdf:subject ?m . ?f id:evidence ?e takes 1.3 s: PostgreSQL evaluates the facts' source union whole instead of per ?m; a constant predicate also compiles branches over sources that cannot hold it. PostgreSQL also estimates 1 row wherever a Zen view finds a predicate by joining knowledge.node on its label (/tmp/carl-sparql/estimate-report.md). Acceptance: the design /tmp/carl-sparql/design-unions.md asks for, built, with its measurements. Depends: none. Claim: agent=carl design (Fable) /tmp/carl-sparql/design-unions.md; measured form B in .jj-ws.noindex/carl-sq-lat.
