---
title: Stop reading the unseen facts tag
status: open
priority: 2
issue-type: task
created-at: "2026-11-18T07:30:24.994140+01:00"
---

Problem: src/core/checker.f CTL-BIT>TAG$ keeps a stage arm that reads the deleted tag `unseen` as bit 62, so an hb whose checker still writes it can build the tree (docs/bootstrap.md staged hosts). Acceptance: once an hb without the tag is installed, the arm is gone and a build from that hb converges. Depends: habu-carry-a-may-c9f6fc75 landed and installed. Claim: unassigned.
