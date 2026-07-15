---
title: "Core: grow compiler source arena"
status: active
priority: 1
issue-type: task
created-at: 2026-07-15T11:07:31.347502+02:00
---

Full context: exact remote-master gate test/gate-stdlib.f is red in tools/build-fixpoint-test.f SOURCE-BOUNDARY after recent compiler/library source growth. The live generated compiler no longer fits SOURCE-ARENA-CAP 0x200000: POLICY first proves cap is smaller than required, then NEXT-POW2 requires 0x400000. This blocks rebasing otherwise-green saved-program work. Re-run the existing bounded exponential plus binary source probe on the exact latest master, report the measured live requirement and headroom, update the single capacity owner and bootstrap/native mirrors to the smallest power of two preserving the documented 25 percent policy, keep stage2/maker/native capacities derived from that owner, add a regression that grows representative cold-prefix source past the old 2 MiB boundary and proves cap-plus-one rejection, run bootstrap-codegen/no-binary/fixpoint/hb-build/source-boundary/full stdlib/test/maki gates, and document the new measured baseline. Do not shrink source, bypass the check, or hardcode a test-only expectation. Claim: agent=source-cap workspace=.jj-ws/habu-core-grow-compiler-33e07ccc.
