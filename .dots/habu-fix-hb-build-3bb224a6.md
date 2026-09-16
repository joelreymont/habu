---
title: Fix hb-build AOT restore-span address refusal
status: open
priority: 2
issue-type: task
created-at: "2026-09-16T11:24:03.747660+03:00"
---

Problem: test/gate-aot-positive.f forks hb-build AOT preseed and hb-build AOT bundle/data, both die aot: address refers to data outside the restored span, exit 74 (src/habu/aot-lib.f), on the pinned engine fE and on the guard-free engines, so SUITE native-gate-aot-positive is red. Cause not yet found: an address cell points outside the captured DATA span, likely a table sized or placed after a layout move. Acceptance: cause named and fixed at its layer, both forks green, gate suite green. Files: src/habu/aot-lib.f, test/gate-aot-positive-lib.f. Verify: bin/hb --load test/gate-aot-positive.f. Depends: none. Ownership: hazel line. Claim: unassigned.
