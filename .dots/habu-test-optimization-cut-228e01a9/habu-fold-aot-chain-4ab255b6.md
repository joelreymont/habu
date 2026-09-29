---
title: Fold aot-chain producer rows onto one host and one capture
status: active
priority: 1
issue-type: task
created-at: "\"2026-09-29T18:01:21.026480+02:00\""
---

Problem: aot-chain-producer, -location and -target build three identical hosts and run nine full compiler captures to make nine in-memory row changes (~19 s per case). Evidence and design: ~/.cache/tmp/kestrel-gate/test-review/L3-aot-image.md finding 2: one host, one capture, fork per ALTER (GE-EVAL-FORK-BAD pattern as test/stripped-address.f). Acceptance: every producer refusal and accepting control keeps its assertion and refusal code; location and target fold into one row unless the pooled time would pass 180 s. Files/Ownership: test/aot-chain-*.f, their test/gate-stdlib-cases.f rows. Base: 614ae0ba (row-split stack head, not yet on master). Verify: every touched row passes standalone (bin/hb --load <row file>); a mutation of one moved or rewritten assertion fails; report per-row seconds before and after. Depends: none. Claim: agent=kestrel/worker workspace=.jj-ws/habu-fold-aot-chain-4ab255b6
