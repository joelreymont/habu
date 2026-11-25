---
title: Give back the tail each catch staled
status: open
priority: 2
issue-type: task
created-at: "2026-11-19T14:36:49.435766+02:00"
---

Problem: under the stale-open-row rule (docs/forth.md:2776-2783), a catch whose open tail is already stale leaves it as it is, so only the first catch's success arm gives the tail back and a word with two catches over one tail is rejected though right. Acceptance: each exact catch's success arm leaves the open tail as it was before that catch, stale or not, and a case of two catches shows it. Depends: none. Claim: unassigned.
