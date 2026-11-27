---
title: Expand a SPARQL path alternation as one union
status: open
priority: 2
issue-type: task
created-at: "2026-11-27T20:11:20.846968+01:00"
---

Problem: src/lib/sparql/emit.f EXPAND, its | branch: p|q|r parses left-nested and expands as a UNION whose first branch is the UNION of p and q, a derived table without per-branch columns, so STEPS cannot step a walk (p|q|r)+ by its native column and compares terms at each step. Acceptance: a | chain of n members expands into one union of n branches, as {s p o} UNION {s q o} UNION {s r o} does; a walk over three alternatives seeded by a constant steps by its native column; answers unchanged on sparql-test.f and w3c-test.f. Depends: none. Claim: unassigned.
