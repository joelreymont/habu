---
title: Leave no address in a destroyed queue
status: open
priority: 2
issue-type: task
created-at: "2026-11-19T17:20:33.524097+01:00"
blocks:
  - habu-keep-an-addr-82425261
---

Problem: src/lib/queue.f Q-SEMS-DROP frees a queue's TASK:sem handles but leaves them in the saved row Q-SEMS, so once TASK:sem is an address a file whose top level destroys a queue fails E-ADDRESS on a private cell. Acceptance: Q-SEMS is an option<TASK:sem> row set to none as each handle goes back; a destroyed queue builds, a live one is rejected E-ADDRESS. Depends: habu-keep-an-addr-82425261. Claim: unassigned.
