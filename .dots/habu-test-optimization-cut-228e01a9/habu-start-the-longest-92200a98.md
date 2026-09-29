---
title: Start the longest gate rows first
status: active
priority: 2
issue-type: task
created-at: "\"2026-09-29T13:58:20.601217+02:00\""
---

Problem: the pool starts rows in registry order, each waiting for a free slot (GT-POOL-START, test/gate-pool.f:1214-1222). In native-gate.log the ten rows of 103-293 s (build-fixpoint-fixtures, hb-build-fixtures, hb-build-stripped, hb-build-stripped-cells, aot-chain-capture, native-window-owner, checker-scan-index, aot-wide-format, stripped-entry, aot-named-cells-image; PASS lines 411-1104) sat mid-registry, started minutes in and alone set the tail; the GROUP SEQ native-serial-gates block mid-registry drains the pool on entry (GROUP-HEADER, lib/test/suite.f:270-277) and then runs 49 parallel rows after it on a fresh pool. Acceptance: those ten rows lead test/gate-stdlib-cases.f in duration order with a comment stating the rule; the sequential group sits last, just before RUN, with its rows and their comments unchanged; the registry holds the same rows and the same row lines. Files: test/gate-stdlib-cases.f. Verify: compare the sorted non-comment lines against the base; the gate run shared with the sibling dots shows the heavy rows starting in the first minute and the sequential group after the last parallel row. Depends: none. Ownership: test/gate-stdlib-cases.f row order. Claim: agent=kestrel workspace=.jj-ws/gate-latency.
