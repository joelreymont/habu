---
title: "Check and load reject a non-required library's host and FUNCTION differently"
status: open
priority: 2
issue-type: task
created-at: "2026-11-25T15:32:56.072593+01:00"
---

Problem: a top-level call of a non-required library's `host` is E-UNREQUIRED at 2:1 by the load and E-SECTION-WORD at 2:1 by the check (src/habu/verify-source.f), and a `FUNCTION:` used without its require makes the check add a second record for `;FUNCTION` at 1:31 beside the load's one. Both reject (rc 70), so only text and count differ. Acceptance: the check prints the load's record alone for both, with a case in test/require-check.f that fails before. Depends: the require feature (habu-reject-a-file-3120d5e1). Claim: unassigned.
