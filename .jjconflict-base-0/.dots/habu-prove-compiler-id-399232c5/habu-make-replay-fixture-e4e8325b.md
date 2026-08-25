---
title: Make replay fixture and schema tell the truth
status: open
priority: 3
issue-type: task
created-at: "2026-07-28T16:00:36.076606+02:00"
---

Full context: destruction review findings 6 and 10, MEDIUM/LOW. (a) test/compiler/ir-id-replay.f's first two arms do not attempt a replay: INCLUDE-SNAPSHOT-PREPARE (src/core/include.f:328) resets REQUIRE-BASE but leaves REQUIRE-N, so REQUIRE-KNOWN? still skips the require - probed empirically with a side-effect file (ran exactly once). The real property is proved solely by the third forced arm (HABU_IR_ID_REPLAY_FORCE), which is genuine and load-bearing; arms 1-2 prove only that NEW-MODULE keeps advancing across ordinary duplicate requires, and the fixture's comment misdescribes the mechanism. Fix the comment to state what each arm proves, or make arms 1-2 genuinely re-include, whichever is honest and minimal. (b) test/compiler/ir-id-schema.f:551 declares fixture row 1 as 'require-replay child-load test/compiler/ir-id.f IR-ID-TEST RELOAD-STABLE$' but the parity gate's PHASE-REPLAY runs test/compiler/ir-id-replay.f, which the canonical schema does not mention - the frozen description and the executed fixture are different artifacts. Make the schema row name the fixture the gate actually runs (a digest change, re-freeze with audit). Also from finding 5: the assumptions report presents cell alignment as modelled, but IdAllocator.aligned is a tautology (machine-checked: forall c, aligned c = true - the model cannot express a misaligned cell); the real guarantee is the frozen Habu alignment prologue. Correct the report and the model comment to say so. Acceptance: comments and schema match executed reality; digest re-frozen with the audit in the commit message; gate green.
