---
title: A does> clause that only runs its quotation argument refuses a reading quotation
status: open
priority: 2
issue-type: task
created-at: "2026-11-11T09:39:00.444419+02:00"
---

Problem: a created word's quotation input always counts as kept: src/core/checker.f CREATED-RECORD-BUILD runs `din KEEP-QUOTS` (the a2c fold on the quotation stack), so a does> clause that only runs its argument and stores it nowhere still refuses a quotation that reads a local. Found by the tests lane (5517b236). A waiting case of the quotation feature, not a prerequisite of Tender's dot. Acceptance: a definer whose does> clause only executes its quotation input accepts a reading quotation; one that stores or returns it still refuses with the prose 'a quotation that reads a local would outlive it'. Files: src/core/checker.f (CREATED-RECORD-BUILD and the does> clause's checked effect), a case beside the tests lane's created-word keeper. Verify: the two cases on an engine built from the lane. Depends: the quotation stack lands (habu-give-a-quotation-304dcb8d). Ownership: those lines. Claim: unassigned.
