---
title: Give the hb-build-fixtures gate row headroom over its 360 s slot
status: open
priority: 3
issue-type: task
created-at: "2026-09-22T18:40:25.038846+03:00"
---

Problem: SUITE hb-build-fixtures (test/gate-stdlib-cases.f:1700-1705: tools/hb-build-test.f, lib/build-cache-test.f, lib/codesign-test.f, tools/hb-build-direct-lints-test.f) runs 353-355 s standalone against SUITE-TIMEOUT-MS 360000 (test/gate-stdlib-lib.f:12); with two or more lanes' suites in flight it reds with kind=TIMEOUT-UNDER-LOAD (three of five full runs under that load, every other row green). Acceptance: the row has at least twice its measured time in headroom - split into rows that each finish under 180 s, or a per-row deadline the SUITE block declares with the measured time beside it; two concurrent full runs have no TIMEOUT-UNDER-LOAD. Files: test/gate-stdlib-cases.f, test/gate-stdlib-lib.f. Verify: two test/run.f runs concurrently. Ownership: hazel. Claim: unassigned.
