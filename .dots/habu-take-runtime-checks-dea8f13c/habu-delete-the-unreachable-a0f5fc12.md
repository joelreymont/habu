---
title: Delete the unreachable internal dies in the core files
status: closed
priority: 2
issue-type: task
created-at: "2026-11-23T18:44:54.562109+01:00"
closed-at: "2026-11-24T15:31:16.806018+01:00"
---

Problem: 24 'EXIT-STATUS:INTERNAL die' sites in src/core type-family, sumtype, render, check-walk, include, check-hook guard conditions no input reaches (every caller keeps them false), so default code runs a check AGENTS.md Changes rules out; census .out/census/dies-b.md, class DELETE. Acceptance: those sites are gone, each one's reach claim confirmed from its callers; engine builds converge. Depends: none. Claim: agent=worker workspace=.jj-ws.noindex/jerry-diesB.
