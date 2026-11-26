---
title: Make the signal facility a named machine
status: open
priority: 2
issue-type: task
created-at: "2026-11-26T13:59:45.484749+01:00"
blocks:
  - habu-lose-no-signal-efebabca
---

Problem: src/lib/signal.f keeps READY, CAUGHT, STUBBED, the fd word and the stubs' in-flight count as flags set in INIT, CATCH, IGNORE, RELEASE and CHILD-RESET and tested elsewhere, the stub included; the review of the resend lane found a transition nobody listed (a forked child copying a nonzero count after RELEASE). Acceptance: signal.f states the facility's states, inputs and transitions in one place, every word moves it only through them, and each transition the stub can race is listed with what keeps it safe. Depends: habu-lose-no-signal-efebabca landing. Claim: unassigned.
