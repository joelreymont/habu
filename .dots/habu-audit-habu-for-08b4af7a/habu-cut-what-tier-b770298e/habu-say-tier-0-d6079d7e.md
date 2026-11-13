---
title: Say tier 0 where docs and tests say JIT
status: closed
priority: 2
issue-type: task
created-at: "2026-11-13T17:57:04.296922+01:00"
closed-at: "2026-11-13T18:32:45.767058+01:00"
---

Problem: docs (forth.md, forth-card.md, debugging.md, xcomp.md), test names and comments, and source comments (checker.f and type-family.f's pass-2 text, fetch-check.f, wasm kernel-words.f, habu2.f, layout.f, fork-child.f) still describe the JIT, tier 1, HIR or a second pass nothing runs; tools/tier-bench.f measures only tier 0. Acceptance: each is made true of today's code or deleted; a consumer-matched name stays and is reported. Depends: none. Claim: agent=worker workspace=.jj-ws.noindex/jerry-t1m.
