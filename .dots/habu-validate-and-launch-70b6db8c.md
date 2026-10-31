---
title: Validate and launch children with one environment
status: open
priority: 2
issue-type: task
created-at: "2026-10-26T12:20:57.277154+02:00"
---

From the external plan W3 (I3), after the stack. Child overrides are built at tools/build-support.f:164 but AOT validates recorded environment rows against the parent's environment (tools/hb-build-lib.f:1057). Build one effective child environment and use it for both validation and launch, as the engine path's CHECK-WITH does; keep absent vs empty distinct. Acceptance: an e2e where the parent and child environments differ selects the right result. No name filters, no environment cache. Depends: stack landed. Claim: unassigned.
