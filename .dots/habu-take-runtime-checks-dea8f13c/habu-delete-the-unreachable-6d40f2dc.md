---
title: Delete the unreachable internal dies in checker.f
status: closed
priority: 2
issue-type: task
created-at: "2026-11-23T18:44:54.549458+01:00"
closed-at: "2026-11-24T15:31:16.791529+01:00"
---

Problem: 57 'EXIT-STATUS:INTERNAL die' sites in src/core/checker.f guard conditions no input reaches (every caller keeps them false), so default code runs a check AGENTS.md Changes rules out; census .out/census/dies-a.md, class DELETE. Acceptance: those sites are gone, each one's reach claim confirmed from its callers; engine builds converge. Depends: none. Claim: agent=worker workspace=.jj-ws.noindex/jerry-diesA.
