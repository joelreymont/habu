---
title: Give locals a lexical environment
status: open
priority: 1
issue-type: task
created-at: "2026-09-16T13:54:54.712363+03:00"
---

Problem: locals are block-scoped but have no defined precedence against the dictionary: a local that shadows a builtin fails cryptically, locals shadow words case-insensitively, and natural names (i, code, dup) collide (MISSING.md Foundation B; Tender LESSONS). Acceptance: scope frames pushed and popped per block with innermost-first resolution then the dictionary; a local may shadow an ordinary word within its scope; shadowing a control or structural word is a located error; slots are freed at frame pop and reused; positive fixtures (block-scoped local used mid-control and after exit; i j k code dup as locals) and the negative fixture pass; prototyped on a temporary engine and landed only when the fixpoint rebuilds byte-identically. Files: src/habu/habu2.f locals (LLOC-FIND, LBCAP, C-LOCAL-REF), src/core/checker.f local tracking, test/engine-suite.f, docs/forth.md. Verify: the fixtures, tools/build-fixpoint.f byte fixpoint, test/run.f green. Depends: habu-warn-when-a-8c4d889a. Ownership: engine and checker lanes. Claim: unassigned.
