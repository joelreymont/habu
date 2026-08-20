---
title: GPT-2 production commands do not load
status: active
priority: 2
issue-type: task
created-at: "2026-08-20T13:14:16.836146+02:00"
---

VERIFIED 2026-08-20 (external review stop-ship 1, reproduced): bin/hb --load tools/gpt2.f -- dies rc 70 'in limit?: at IC>N expected item-count actual: bool n a' - maki/infer/gpt2-generate.f:51 stores nominal values through generic constant which records a, losing the type; LIMIT? then reads MAX-TOKENS as item-count. AND the production-entry tests (generation, service, token guard, cleanup) are absent from maki/test.f:306 and maki/test-core.f:256 - passing-is-not-scheduled, live instance. Fix: typed nullary words or a typed-constant definer (probe what layout-buffer/TYPED-VARIABLE now provide post-0cc8d823 before minting anything), then REGISTER production-entry smoke tests in both inventories.

Claim: agent=gpt2-1 workspace=.jj-ws/habu-effstore
