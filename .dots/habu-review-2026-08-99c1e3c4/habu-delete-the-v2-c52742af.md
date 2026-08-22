---
title: delete the V2 design-database complex (TYPE-FIXES item 37)
status: open
priority: 1
issue-type: task
created-at: "2026-08-22T22:38:25.986652+02:00"
---

Problem: maki/db/* (60 files), maki/evidence/policy.f + promote.f, maki/experiment/*, maki/competitive-*.f, journal.f, rev.f, producer.f, config.f, artifact.f, schema.f: 39 non-test files = 13,141 lines plus 52 tests = 13,309 lines with zero production consumers (rg 'require maki/db/' outside the cluster: none; the only live edge is cad.f -> evidence/schema.f for the golden-leg/prec-class enums). TYPE-FIXES-PLAN.md:609-628 item 37 ruled it deleted on 2026-07-30; ~45 suites in maki/test.f:182-234,341-367 spend gate time on it and the -5310..-5620 codes and CAD-KIND families exist only for it. Acceptance: item 37 executed as written: golden-leg/prec-class moved into maki/golden.f first, the cluster deleted with its suites, the error ranges released, maki/test.f green and measurably faster (time recorded). Files: maki/. Verify: maki/test.f; rg shows no reference. Depends: none. Ownership: maki. Claim: unassigned.
