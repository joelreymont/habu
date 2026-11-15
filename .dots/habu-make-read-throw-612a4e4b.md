---
title: Make READ-THROW private
status: open
priority: 2
issue-type: task
created-at: "2026-11-16T00:04:02.304148+01:00"
---

Problem: tools/source-discovery.f READ-THROW sits in a public section, but its only caller is READ-SELECTED in the same file. Acceptance: READ-THROW is private and the suites that load source-discovery.f pass. Depends: habu-campaign-c4-diagnostics-3b6de147. Claim: unassigned.
