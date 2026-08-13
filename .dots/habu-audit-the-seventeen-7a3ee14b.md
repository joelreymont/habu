---
title: Audit the seventeen workspaces holding unmerged commits
status: open
priority: 2
issue-type: task
created-at: "2026-08-13T17:35:43.317918+02:00"
---

Workspace sweep 2026-08-13 removed the empty lanes; seventeen remain whose checkouts hold commits that may be unmerged work or already-landed duplicates. For each: decide merged (forget+rm), dead (abandon+forget+rm), or live (re-dot the work, then forget+rm - a workspace is not a tracker). The list with checkout commits: census-repair a099154e, compile-shap d82c8753, compiler-ir-control 76cde081, dots-retire 3f5761ce, folding 395fd8aa, give-layout 60c6bfe1, habu-cmp-imm 54461eda, habu-recover-checked-pty 56d9f3b2, habu-reject-safetensors c73d531e (DIVERGENT - resolve first), ir0-bootstrap-fix ad17e5a3, jsonratio 50f66ff4, reach-callers 9bae3c8c, reject-a-bar ee3e4f9a, safet-wrap-review-mutation a6d92660, schedlint 86cbf5ec, unmap 3f2f5bc5, xt-plan-integrate 603847a3 + xt-plan-review-r3 76b9d86a + xt-plan-revision 7d260813. Committed work survives forget+rm (it lives in the repo store); the audit is about deciding, not preserving. Files: .jj-ws/. Depends: none.
