---
title: Load resources with generation guards
status: open
priority: 2
issue-type: task
created-at: "2026-10-04T05:10:11.570695+03:00"
---

Problem: HBR2 §7.4 gives resources the states Idle, Loading, Ready, Refreshing(previous) and Failed, starts work only as a command or effect and never inside a view, and matches late results to scope and generation; §7.6's MaterialChooser trace drains generation 2 and accepts generation 3. Acceptance: in package UI-RESOURCE, a resource descriptor owning its request generation and optional previous value; starting or refreshing is an effect issued through habu-dispatch-commands-through-ba481b94 that submits a habu-track-requests-to-d5dfab24 request; a result is adopted only for the current generation and scope, and any other is drained and released; disposal cancels observation and cancellable work, while a load shared with another consumer continues; Refreshing keeps the previous value readable as previous, never as current. Files: lib/ui/resource.f (new, package UI-RESOURCE; mints E-UI-RESOURCE-FIRST/LAST -9730..-9739 in its owning file), lib/errors.f (one comment line), test/browser/resource-test.f (new), test/gate-stdlib-cases.f. Verify: bin/hb --load test/browser/resource-test.f: generations 2 and 3 answering out of order end Ready with generation 3's value and generation 2's handle released; a disposed consumer leaves a shared load running for its peer; bin/hb --load test/run.f. Depends: habu-register-comps-and-73173e51, habu-track-requests-to-d5dfab24. Ownership: lib/ui/resource.f. Lane: tim. Claim: unassigned.
