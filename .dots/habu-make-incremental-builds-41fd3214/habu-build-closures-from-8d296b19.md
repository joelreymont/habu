---
title: Build closures from the requested root
status: open
priority: 2
issue-type: task
created-at: "2026-10-26T12:20:57.321094+02:00"
---

From the external plan W10 (I8), after the stack. tools/chain-plan.f:38 and :100 disagree on which root builds the closure and resolves paths. Use the supplied root for both and reset cached closure state when the root changes. Acceptance: a chain plan for a second root resolves that root's files. Cut the builder-query migration. Depends: stack landed. Claim: unassigned.
