---
title: Check on a Mac that AIO leaves no run state
status: open
priority: 2
issue-type: task
created-at: "2026-11-25T17:58:10.379256+01:00"
---

Problem: src/lib/aio-macos.f's run state is scratch, but no Mac has built a load that ran AIO. Acceptance: test/file-objects.f passes on a Mac. Depends: a reachable Mac. Claim: unassigned.
