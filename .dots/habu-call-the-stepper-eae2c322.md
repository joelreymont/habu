---
title: "Call the stepper's reader from the REPL directly"
status: open
priority: 2
issue-type: task
created-at: "2026-11-18T19:26:17.932313+01:00"
---

Problem: REPL-READ (src/habu/repl.f:5) is a defer only src/habu/stepper.f S-INSTALL (:107-109) ever sets; src/habu/interpreter.f REPL calls it (:3266). A hook with one value is a direct call. Acceptance: REPL calls the stepper's line reader directly (a public DEBUG word); repl.f, REPL-READ and S-INSTALL are gone; the genio suite's REPL-over-a-device case passes. Depends: the d6 lane (vqumrllk) lands. Claim: unassigned.
