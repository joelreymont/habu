---
title: "schedule-lint: resident slices schedule no registration"
status: open
priority: 2
issue-type: task
created-at: "2026-08-17T04:25:41.058097+02:00"
---

tools/lint/schedule-lint.f SLICE-AT (line 543) counts a slice as a live runner whenever a live phase has STDLIB-SLICE? and a PHASE-SLICE-TOKEN, WITHOUT asking TEST:PHASE-RESIDENT?. For a RESIDENT phase the token is fiction: test/run.f forks test/run-worker-stdlib.f, which runs the in-process GSI body and never loads test/gate-stdlib-cases.f, so no registration under that slice runs. Resident-only slices today: tool (phases 2,21,22,23,24,36,37,38,39), check-cli (3), lint-tools (17). Only phases 4, 18, 19, 40 actually spawn 'test/gate-stdlib.f -- <slice>'. MEASURED on the tree 2026-08-17: 233 registrations, 8 covered by label alone under a resident-only predicate; 7 of those (shadow-lint, repl-lint, dot-dep-lint, maki-dep-lint, namespace-lint, error-code-lint, schedule-lint) are run anyway by test/gate-stdlib-lint-tools.f through the tool's own word, and ONE - region-room - was genuinely dark: a broken assertion left the whole battery green (bake-chain-22, lane habu-seeded-words-invisible-c7505a49). Fixed for that suite by giving it a fork in the GSI body. FIX SHAPE: SLICE-AT must skip resident phases (or the lint must read the resident body's fork list for them), and the fixture must carry a resident-only-slice registration that the lint names. Falsify by mutation: with the fix, moving region-room's registration back to label-only coverage must produce a finding.
