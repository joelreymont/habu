---
title: Gate must test maki on the candidate engine
status: active
priority: 2
issue-type: task
created-at: "\"2026-07-19T17:44:18.052517+02:00\""
---

Review finding 3 (pin 8195257e, high confidence): test/run-lib.f:1860 starts the maki slice early against the LITERAL bin/hb (the baseline), not the built candidate; ~20 more literal engine paths in the file. Proven divergence: baseline maki/cad-test.f passes while the candidate fails F100/F101 (durable keys include the running engine hash). Fix: run the maki slice after candidate readiness under HABU_UNDER_TEST and route EVERY child engine invocation through ENGINE-PATH$ (sweep all literal bin/hb paths in run-lib.f). This is a gate-soundness hole: maki green today does not certify the engine being shipped.

Claim: agent=gate-candidate workspace=.jj-ws/habu-gate-must-test-cd70ef4e
