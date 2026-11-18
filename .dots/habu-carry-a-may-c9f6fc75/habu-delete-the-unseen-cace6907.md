---
title: Delete the unseen tag reader
status: open
priority: 2
issue-type: task
created-at: "2026-11-18T08:10:00.101416+01:00"
---

Problem: src/core/checker.f CTL-BIT>TAG$ still reads the tag unseen as bit 62, because an hb whose checker writes unseen (the installed one) builds the candidate; no rule tests the bit and XFER-FLAG-MASK drops it. Acceptance: once an hb built from this stack is installed, the unseen arm and its comment go and a cold build with that hb equals a warm one. Depends: the stack's install. Claim: unassigned.
