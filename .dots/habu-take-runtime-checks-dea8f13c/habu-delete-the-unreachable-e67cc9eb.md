---
title: Delete the unreachable internal dies in the engine, linkers and tools
status: closed
priority: 2
issue-type: task
created-at: "2026-11-23T18:44:54.577867+01:00"
closed-at: "2026-11-24T15:31:16.821367+01:00"
---

Problem: 36 'EXIT-STATUS:INTERNAL die' sites in the other files of sites.txt guard conditions no input reaches (every caller keeps them false), so default code runs a check AGENTS.md Changes rules out; census .out/census/dies-c.md, class DELETE. Acceptance: those sites are gone, each one's reach claim confirmed from its callers; engine builds converge. Depends: none. Claim: agent=worker workspace=.jj-ws.noindex/jerry-diesC.
