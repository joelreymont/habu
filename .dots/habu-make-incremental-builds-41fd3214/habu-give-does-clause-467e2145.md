---
title: Give does> clause records a binding contract
status: open
priority: 2
issue-type: task
created-at: "2026-10-24T04:25:43.327578+03:00"
---

A file whose body calls a does> clause (e.g. trusted MAKE;does) is refused ordered-dependency reason=binding-contract-unknown dependency=MAKE;does: the clause record has no binding contract. With one, the does lane's P1 (clause calls keyed by the clause record, not parent+offset) gets its test: edit MAKE's prelude, consumer reused/missed correctly. Workspace: none yet.
