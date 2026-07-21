---
title: "STRUCTURE: generate MAKE UNMAKE"
status: active
priority: 1
issue-type: task
created-at: "\"2026-07-13T17:13:28.696041+02:00\""
blocks:
  - habu-structure-parse-typed-c5a01e1f
---

Own STRUCTURE constructor generation and focused ctor tests. Generate sealed FAMILY:MAKE and FAMILY:UNMAKE checked effects from declaration-order field schemas, preserve generic substitutions and exact layout width, and publish atomically only after declaration validation. Add positive round trips and arity/type/rollback negatives.

Claim: agent=genmake workspace=.jj-ws/habu-structure-generate-make-872a6e75 (Mac; EARLY START against the landed decl-event + TYPE-FIELD contracts: builds the sealed FAMILY:MAKE/UNMAKE generator in a NEW src/core file with decl-event-driven fixtures, exactly as test/decl-event-suite.f drives declarations. The blocker edge on structure-parse-typed governs CLOSURE, not start: syntax-level STRUCTURE tests and the one-line ;STRUCTURE wiring land in the reconciliation commit after the parse front end merges. Write set disjoint from the structparse lane except append-only assembly/load rows, resolved by the orchestrator at merge.)
