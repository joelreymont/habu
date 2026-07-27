---
title: Scope GPT-2 mapped imports
status: active
priority: 1
issue-type: task
created-at: "2026-07-28T01:49:20.245303+02:00"
---

Problem: the renamed mapped suite is a changed multi-call SAFET consumer. Owner and result: the first GPT2LOAD package in maki/infer/gpt2-mapped-test.f opens one using SAFET after package setup and closes it immediately after T-REAL-MAPPED, before the checker-candidate section. Every executable SAFET call in that interval becomes bare; the outside-test package receives no import. Preserve all type and MATCH selectors, candidate strings in both packages, comments, definitions, call order, and runtime behavior. Dependency: accepted loader rename ae1f14633232efba954e04e76f82b3b73e9e56be. Checkpoint: the exact mapped suite is green; token-aware census proves the repeated SAFET calls and no collision; a representative import passes the package gate. Files: maki/infer/gpt2-mapped-test.f only. Forbidden: an import across candidate evaluation or ;package, changes to the outside-test package, retained executable SAFET qualifiers, API or behavior changes, aliases, string edits, metadata, or other files. Acceptance: exact mapped suite and exact-diff typed-local and package gates pass; the scope is balanced and no executable SAFET qualifier remains in the imported interval.

Claim: agent=gpt2-mapped-using workspace=.jj-ws/habu-scope-gpt-2-d88f2d92
