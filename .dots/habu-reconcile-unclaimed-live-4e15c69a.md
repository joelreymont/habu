---
title: Reconcile unclaimed live workspaces
status: open
priority: 2
issue-type: task
created-at: "2026-07-29T17:14:17.088817+02:00"
---

Full context: eleven workspaces exist that no active dot claims — census-repair, compiler-ir-control, dots-retire, gate-size-entry, habu-reject-safetensors-header-5fd15f8d, ir0-bootstrap-fix, merge-gate, safet-wrap-review-mutation, xt-plan-integrate, xt-plan-review-r3, xt-plan-revision. Several sit on undescribed commits and may hold unmerged work. This is the mirror of the stale-claim problem: 54 active dots carry NO claim at all, and at least three (habu-split-gate-size-4a6fad8b, habu-make-lint-lex-0edc045e, habu-fix-signature-comment-2e17e2b7) are named by live workspace directories, so work is in flight with nothing recorded. Two instances of permanently lost work this week (the frozen rename diff, and commit a2c4ec40 for habu-fix-owner-wid-e2bc360c) came from exactly this gap. For each workspace: inspect its commits, land or discard explicitly, then retire it. Then require the claim line at dispatch so an unclaimed workspace is itself a finding.
