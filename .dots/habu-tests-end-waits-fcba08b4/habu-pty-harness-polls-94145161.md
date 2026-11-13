---
title: pty-harness polls to a clock and keeps an uncalled word
status: open
priority: 2
issue-type: task
created-at: "2026-11-13T20:51:23.712656+01:00"
---

Problem: src/lib/pty-harness.f READ-STEP (:159-162) polls POLL-MS and answers 0 so its callers loop to WAIT-OPEN's deadline (:174-175), a clock wait; READ-TO-EOF (:168-169) is public and nothing calls it. Acceptance: the harness reads with no deadline; READ-TO-EOF is deleted; the pty suites pass. Depends: the waits campaign commit. Claim: unassigned.
