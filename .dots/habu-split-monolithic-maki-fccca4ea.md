---
title: Split monolithic maki suite into parallel gate slices
status: active
priority: 2
issue-type: task
created-at: "\"2026-07-19T21:03:07.024841+02:00\""
---

The gate's cold long pole is the single 'native maki checked suite' child running all of maki/test.f serially: 27.7s -> 30.8s on 2026-07-19 alone (framing, cad-replay, affine-LN op, equation-adjoint suites landed in one day), forcing the cold budget re-derivation 34000 -> 46000 in test/run-lib.f. Every future model-op lane grows it further; budget bumps track the cost but do not contain it. Proper fix: split maki/test.f into parallel pool slices like the native test families (test/run-lib.f phase pool GT-POOL-START) - natural cut points: core ops/executor, backward+gradcheck (the fd-heavy half), store/cad, device-smoke+lowering. Constraints: the suites share one image today (case-insensitive name collisions, load-order deps like backward-test's fresh-BW-state precondition) - each slice becomes its own child image, which REMOVES the collision trap for future lanes as a bonus. Acceptance: cold gate long pole drops materially (target: maki slices each under ~15s), all suites still run, coverage lint still accounts every maki test file to exactly one slice, budgets re-derived DOWN with the measurement. Territory: maki/test.f (split into slice files), test/run-lib.f (slice registration), suite-coverage lint rows.

Claim: agent=makisplit workspace=.jj-ws/makisplit machine=spark
