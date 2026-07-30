---
title: Disposition of orphan src/core/sha-check.f
status: open
priority: 2
issue-type: task
created-at: "2026-07-30T02:50:42.439754+02:00"
---

Why: sha-check.f is a self-test no loader references anywhere in the tree, and its DIFF word fails the checker identically on master (pre-existing, measured during the ptr-elem sweep). Dead code that cannot run. Behavior: either delete the file or wire it into the owning sha256 test path with its checker rejection fixed — decide by whether sha256.f has live test coverage elsewhere. Owner: src/core/sha-check.f. Dependencies: none. Acceptance: no orphan remains — file deleted or loaded green by a registered suite. First consumer: test/run.f's sha coverage. Claim: unassigned.
