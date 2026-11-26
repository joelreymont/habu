---
title: "Answer Tender's similar lots in time"
status: open
priority: 2
issue-type: task
created-at: "2026-11-25T15:22:13.319616+01:00"
---

Problem: Tender's LIST-QUERY (tenderd/similar.f) plans in 5 s and runs up to 102 s against 30 ms for the SQL it replaced, past Zen's 5 s limit, and lists a lot whose only shared scope words are numbers (lot 1995 lists 2006), which similar.f promises to drop. Acceptance: LIST-QUERY is a set-based SPARQL query, test/tenderd/similar-test.f passes and each lot of its data gets the old SQL's answer. Depends: none.
