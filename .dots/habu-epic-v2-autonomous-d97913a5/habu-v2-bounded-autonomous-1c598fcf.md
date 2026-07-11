---
title: V2 bounded autonomous agent loop
status: open
priority: 1
issue-type: task
created-at: "2026-07-11T12:25:28.119666+02:00"
blocks:
  - habu-v2-agent-proto-2ed8c846
---

Implement the controller that repeatedly inspects revision/diagnostic/obligation state, enumerates registered legal actions, applies one budgeted transaction, runs focused verification, measures progress, and promotes/continues/reverts/returns typed blocked. The LLM is an untrusted chooser behind the action protocol. Acceptance: replay without the LLM yields identical state; raw command/edit cannot bypass registry; injected non-progress terminates; crash/retry is idempotent; authority and budgets hold.
