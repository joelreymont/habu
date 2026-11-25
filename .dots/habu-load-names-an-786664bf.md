---
title: Load names an immediate in a does> clause
status: open
priority: 2
issue-type: task
created-at: "2026-11-25T15:59:32.563050+01:00"
---

Problem: an evaluating immediate inside a does> clause is E-UNDEFINED at the load, with the text `does>` is a keyword, not a word; the check says E-UNMODELED-IMMEDIATE, as both do in a plain body, and the clause without the immediate loads. Acceptance: both give E-UNMODELED-IMMEDIATE at the immediate. Depends: none. Claim: unassigned.
