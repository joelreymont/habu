---
title: Generate the checker parity vector table
status: open
priority: 2
issue-type: task
created-at: "2026-07-29T19:29:19.804672+02:00"
---

Full context: the behavioural half of test/compiler/checker-model-proof.f compares TEN hand-picked programs through both the real checker (CHECK-QUIET-CANDIDATE) and the model (check_ctl). Ten programs cannot distinguish the two machines agreeing from the two machines agreeing HERE. Generate the table instead: every ordered pair of the 30 concrete types through the widening lattice, every case and MATCH arm shape, every one of the ten control frame kinds opened and closed, each driven through both machines from one shared row. Keep the single-verdict-cell discipline — neither side may hold its own copy of the expected answer. Acceptance: the generated table is materially larger, still runs inside the gate's time budget, and perturbing any generated row reds both sides.
