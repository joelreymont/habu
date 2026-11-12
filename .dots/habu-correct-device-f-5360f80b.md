---
title: "Correct device.f's pointer to a run doc"
status: open
priority: 2
issue-type: task
created-at: "2026-11-12T11:17:20.355030+02:00"
---

Problem: test/wasm/device.f:4 sends the reader to docs/bootstrap.md for running the wasm device check, but bootstrap.md never mentions device.f, wasm-tools or bun; device.f's own header (lines 1-4) is the only statement of the run. Found from Maki by a census on 2026-11-12. Acceptance: device.f's header names a doc that states how to run the check, or drops the pointer and states the run itself. Depends: none. Claim: unassigned.
