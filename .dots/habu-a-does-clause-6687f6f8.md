---
title: A does> clause that only runs its quotation argument refuses a reading quotation
status: open
priority: 2
issue-type: task
created-at: "2026-11-11T09:39:00.444419+02:00"
---

Problem: a created word's quotation input always counts as kept: src/core/checker.f CREATED-RECORD-BUILD (:16362) runs `din KEEP-QUOTS` (:16380), so a does> clause that only runs its argument and stores it nowhere still refuses a quotation that reads a local. Not a prerequisite of Tender's dot. Acceptance: a definer whose does> clause only executes its quotation input accepts a reading quotation; one that stores or returns it still refuses with the prose 'a quotation that reads a local would outlive it'. Depends: none. Claim: unassigned.
