---
title: Honor set-current publication scope
status: active
priority: 1
issue-type: task
created-at: "\"2026-07-14T15:56:27.195515+02:00\""
blocks:
  - habu-reject-duplicate-arm64-8f9565fe
---

Full context: src/core/checker.f package definition tracking records the active package scope even when checked source executes set-current 0 before defining a global word. Runtime publishes the word in WID 0, but the checker stores its signature under the package, causing generated-stage E-UNDEFINED and forcing an avoidable TRUST row. Root fix: make definition publication derive signature ownership from the actual current WID selected by set-current, while preserving package visibility/seal rules and rollback. Add a minimal checked regression that opens a package, sets current to WID 0, defines a global word calling a private helper, proves the global signature resolves and the private helper remains inaccessible bare/qualified; add inverse package-current and invalid-WID negatives; preserve bootstrap/native parity. Blocks duplicate ARM64 label rejection because LBL, requires this fail-closed private-helper/global-publication shape. Files: checker definition publication, bootstrap mirror if owned, focused package/checker tests, docs/LESSONS as warranted.

Claim: agent=/root/checker_current_wid workspace=.jj-ws/checker-current-wid.
