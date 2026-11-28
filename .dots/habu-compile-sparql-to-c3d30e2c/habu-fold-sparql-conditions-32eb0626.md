---
title: Fold SPARQL conditions known at compile time
status: closed
priority: 2
issue-type: task
created-at: "2026-11-26T23:27:29.321492+01:00"
closed-at: "2026-11-28T05:23:07.321771+01:00"
---

Problem: a walk's node test (NEW-NODE-TEST, src/lib/sparql/emit.f) writes two EXISTS per source of the active graph, and an EXISTS whose conditions another EXISTS of the test implies adds only planning: on store-a 21 of the node test's 70. Acceptance: a node test holds no EXISTS another of its EXISTS implies; every SPARQL test answers as before; no measured shape's statement grows. Depends: none.
