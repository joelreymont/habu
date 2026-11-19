---
title: Give back the tail each catch staled
status: open
priority: 2
issue-type: task
created-at: "2026-11-19T14:36:49.435766+02:00"
blocks:
  - habu-refuse-a-linear-489a13c2
---

Problem: under the stale-open-row rule (dot habu-refuse-a-linear-489a13c2), a catch whose open tail is already stale leaves it as it is, so only the first catch's success arm gives the tail back and a word with two catches over one tail is refused though right. Acceptance: each exact catch's success arm leaves the open tail as it was before that catch, stale or not, and a case of two catches shows it. Depends: habu-refuse-a-linear-489a13c2. Claim: unassigned.
