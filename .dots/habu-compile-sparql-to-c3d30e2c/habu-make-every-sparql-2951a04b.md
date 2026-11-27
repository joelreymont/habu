---
title: "Make every SPARQL rejection's fix progress"
status: closed
priority: 2
issue-type: task
created-at: "2026-11-27T11:13:51.434223+01:00"
closed-at: "2026-11-27T14:36:25.581368+01:00"
---

Problem: a SPARQL rejection that asks to write a token before one that cannot follow it repeats when its fix is followed: `GROUP_CONCAT(?o; ORDER BY ?s FOO)` says "write a term such as ?x or <iri> here" at FOO again after each ?x written (Joel 2026-11-14: a rejection prints the fix, so the agent can fix the query). Acceptance: following any rejection's fix literally either ends in an accepted query or in a rejection that names a different place or fix; a test follows the chains of the syntax tests' rejections. Depends: none.
