---
title: V2 bounded autonomous agent loop
status: closed
priority: 1
issue-type: task
created-at: "2026-07-11T12:25:28.119666+02:00"
closed-at: "2026-07-18T01:36:07.166504+02:00"
close-reason: "Bounded controller landed (cfceb8be): untrusted chooser behind the action protocol, five acceptance legs proven, zero new trust. V2 autonomous implementation chain complete."
blocks:
  - habu-v2-agent-proto-2ed8c846
---

Implement the controller that repeatedly inspects revision/diagnostic/obligation state, enumerates registered legal actions, applies one budgeted transaction, runs focused verification, measures progress, and promotes/continues/reverts/returns typed blocked. The LLM is an untrusted chooser behind the action protocol. Acceptance: replay without the LLM yields identical state; raw command/edit cannot bypass registry; injected non-progress terminates; crash/retry is idempotent; authority and budgets hold.

Claim: agent=aloop workspace=.jj-ws/fable-aloop (owns new maki/db agent-loop files + tests)

RESOLVED 2026-07-18 (aloop lane, commit cfceb8be): ALL FIVE acceptance
legs test-proven. Package ALOOP: ENUM-AT/DISPATCH-filtered frontier,
attenuated-grant + ledger COMMIT-AUTHORIZED as the SOLE mutation,
APPLIC-driven typed progress metric, three independent termination
bounds, journaled decisions by idempotency key, loop-result custom sum.
The chooser is an untrusted checked quotation - raw commands are
STATICALLY untypeable (verdict fixtures) and dynamic bypasses reject
with HEAD unchanged. Replay-without-chooser identical (key+HEAD+ledger
digest); crash/retry charges exactly once; attenuation escapes reject
with zero commits. Zero new trust. Design seam recorded: effect
authority rides CFG-EFF per action.f's documented model (grant carries
cap+budget only). NOTE: maki suite table is now at exactly 128/128 -
the capacity wall is minted as its own dot.
