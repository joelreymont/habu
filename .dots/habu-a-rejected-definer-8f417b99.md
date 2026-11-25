---
title: A rejected definer records nothing more in the check
status: open
priority: 2
issue-type: task
created-at: "2026-11-25T15:59:32.572581+01:00"
---

Problem: after the check rejects a top-level definer (E-UNREQUIRED) its arm still runs, so the check also reports E-BAD-STORAGE or E-FFI-SYNTAX where the load prints only the first. The fix site is RUN-ARM in TOP-DEFINER? (src/habu/verify-source.f); silencing the arm also drops the tally that opens the stretch. Acceptance: the check prints the load's record alone. Depends: none. Claim: unassigned.
