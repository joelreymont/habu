---
title: "Find why Tender's classify suite fails with model -9705"
status: open
priority: 2
issue-type: task
created-at: "2026-11-26T14:46:21.312787+01:00"
---

Problem: Tender's tenderd-classify suite fails 16 cases on arm64 and x86-64 alike: worker tasks end 'model -9705' (src/lib/net/model.f:132, a malformed or inconsistent provider response or replayed journal record), against a stub and a real model service. Acceptance: the cause is found and fixed where it lies (Habu or Tender's owner), and the suite passes. Depends: none. Claim: unassigned.
