---
title: "Add seven standard words to the card's absent table"
status: active
priority: 3
issue-type: task
created-at: "2026-11-10T10:28:37.026914+02:00"
---

Problem: docs/forth-card.md:231-233 lists classic words Habu lacks and what to write instead. Measured 2026-11-10 over the Claude Code transcripts of habu, maki, tender and zen since 2026-11-01 (453,140 tool calls), agents wrote and then had to fix these words, which the table does not list: `0>` 45 times, `hex` 24, `.r` 9, `to` 9, `2*` 7, `0>=` 5, `cmove` 4; a trailing `bye` was refused 284 times. Report and counts: ~/Work/reviews/habu-10x-2026-11-10/measure-f2.md, measure-f2/std.tsv. Acceptance: the table gains a row for each of `0>`, `0>=`, `2*`, `hex`, `.r`, `to`, `cmove` and `bye`, each naming what Habu offers, shown to work by running it; docs/forth.md's matching list follows where it has one. Files: docs/forth-card.md, docs/forth.md. Verify: each replacement loaded with hb. Depends: none. Ownership: the card. Claim: agent=bob workspace=.jj-ws.noindex/habu-add-seven-std-5dc9c59b
