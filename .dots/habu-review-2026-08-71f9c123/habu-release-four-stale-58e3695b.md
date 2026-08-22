---
title: release four stale active claims
status: closed
priority: 2
issue-type: task
created-at: "2026-08-22T22:38:25.813855+02:00"
closed-at: "2026-08-22T23:59:56.814961+02:00"
close-reason: the four leaves released in place with the RELEASED 2026-08-22 line; dot ls --status active shows only live workspaces; the two PREMISE FALSIFIED leaves keep their text for the owner ruling they still need.
---

Problem: four leaves are 'active' with claims on workspaces that no longer exist (jj workspace list shows only default, recovery-bisect, recovery-mirror): habu-cut-colon-compilation-a5aa3f1f (workspace .jj-ws/habu-thecut), habu-typed-storage-sweep-b2cd1a61 (.jj-ws/habu-thecut), habu-visibility-discharge-548-fab55650 (.jj-ws/habu-trusted), habu-cast-definer-330-1f5980b8 (.jj-ws/habu-trusted). The 2026-08-21 gc keyed on claim lines naming the dot id and missed shared-workspace names. Two of them record PREMISE FALSIFIED and need a ruling, not a worker. Acceptance: each leaf gets 'RELEASED 2026-08-22: workspace gone, no live lane', status open, Claim unassigned; the two falsified ones re-scoped or closed with reason. Files: the four leaves. Verify: dot ls --status active shows only live workspaces. Depends: none. Ownership: tracker. Claim: closed.
