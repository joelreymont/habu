---
title: Build hb-build fixtures in process, not by spawning the tool
status: active
priority: 1
issue-type: task
created-at: "\"2026-09-29T18:01:21.006280+02:00\""
---

Problem: the gate spawns tools/hb-build.f 31 times; each spawn pays 13.7 s of tier-1 compilation before argv is read. Only 6 calls test the CLI itself (--report-json, path-error JSON, one lint, one maker refusal propagation, --preseed); the other 25 test the maker child or the built image. Evidence and call list: ~/.cache/tmp/kestrel-gate/test-review/L8-tools.md finding 1. Also cannot-fail assertions: tools/hb-build-test.f:81-82 (REBUILD-REPL trace flags never set; third identical REPL build) and tools/hb-build-large-source-test.f:124-127,141-143 (files checked at a path the CLI never uses). Acceptance: the 25 non-CLI calls use the production words the test process already holds (HBB-RUN-MAKER-CMD, in-process HBB-BUILD, as tools/hb-build-aot-test.f does); the 6 CLI calls stay spawned; every assertion keeps detecting its failure; the two cannot-fail checks assert something that can fail or are removed. Files/Ownership: tools/hb-build-*test*.f, tools/hb-build-test-lib.f, lib/build-cache-test.f, tools/image-size-lib.f. Base: 614ae0ba (row-split stack head, not yet on master). Verify: every touched row passes standalone (bin/hb --load <row file>); a mutation of one moved or rewritten assertion fails; report per-row seconds before and after. Depends: none. Claim: agent=kestrel/worker-max workspace=.jj-ws/habu-build-hb-build-43939b3e
