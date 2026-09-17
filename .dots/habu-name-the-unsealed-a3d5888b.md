---
title: Name the unsealed engine at the head of whitebox suites
status: open
priority: 3
issue-type: task
created-at: "2026-09-17T17:38:27.930510+03:00"
---

Problem: test/gate-stdlib-cases.f declares some suites WHITEBOX-SUITE, which the gate runs against the unsealed image test/whitebox-engine.f builds (lib/test/suite.f WHITEBOX-RUNNER, test/gate-stdlib-lib.f SUITE-WB-RUN); run standalone against the sealed product engine they red for the wrong reason (measured 2026-09-17 on d5e871c0 and c28d1c93: bin/hb --load test/app-image.f = 6 TFAIL, asserts 76-81, 'hb: internal engine word: DECLARATIONS', with or without the gate's environment). test/whitebox-engine-suite.f says so in its header; test/app-image.f and any other whitebox suite that does not is a trap for whoever runs it by hand. Acceptance: every file declared WHITEBOX-SUITE opens with a Run: line naming the unsealed engine and the gate that supplies it, and the standalone run refuses early by name (one check at the top: the engine is sealed, so say 'app-image: needs the unsealed whitebox engine (gate-stdlib-cases WHITEBOX-SUITE)' and exit 70) instead of six assertion reds. Files: the whitebox suite files test/gate-stdlib-cases.f lists, lib/test/ if the check is shared. Verify: standalone run prints the refusal; the gate still runs them green. Depends: none. Ownership: test harness. Claim: unassigned.
