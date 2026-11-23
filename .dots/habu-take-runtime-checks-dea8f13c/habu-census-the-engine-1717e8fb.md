---
title: "Census the engine's internal-error dies"
status: closed
priority: 2
issue-type: task
created-at: "2026-11-23T18:36:13.900543+01:00"
closed-at: "2026-11-23T18:45:20.516248+01:00"
---

Problem: 244 sites in src/ and tools/ end in 'EXIT-STATUS:INTERNAL die' (src/core/checker.f 93, type-family.f 33, sumtype.f 15, render.f 10, the rest under 10 each), e.g. checker.f:18976 'bad width-fact index'. Each runs in every default engine and guards against hb's own error, which root AGENTS.md Changes rules out; the linker's own-state assertions already went (dot 2322c351). Acceptance: a table per file of every site: path:line, what it guards, whether any input reaches it, and what it becomes (deleted as unreachable, a structure that cannot reach it, a rejection the user can fix, or growth in place of a fixed cap); each class becomes a deletion lane. Depends: none. Claim: agent=jerry workers (read only, tables in /tmp/jerry-p1/dies/).
