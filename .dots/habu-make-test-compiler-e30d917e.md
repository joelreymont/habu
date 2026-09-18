---
title: Make test/compiler/native-trap.f pass when loaded alone
status: open
priority: 3
issue-type: task
created-at: "2026-09-18T20:37:50.696224+03:00"
---

Problem: test/compiler/native-trap.f fails four asserts (21, 23, 24, 25 in its own numbering) when loaded standalone with bin/hb --load test/compiler/native-trap.f, on the release engine and on the base tree ced84910 alike (measured by the effect-schema lane, /tmp/hazel-neff/base); under test/run.f the same suite PASSES, so the standalone route differs from the gated one (a fixture that another suite of the same SUITE block leaves behind, or an ambient argv/tier difference - gate.md 'How a suite runs'). Acceptance: identify what the gated run supplies that the standalone load does not and fix the responsible layer (the fixture, or the suite's own setup) so the file passes alone and under the gate with the same assertions; no assertion weakened. Files: test/compiler/native-trap.f (and the fixture it depends on). Verify: bin/hb --load test/compiler/native-trap.f; bin/hb --load test/run.f (compiler-native-trap green). Depends: none. Ownership: test/compiler/native-trap.f. Claim: unassigned. A second instance of the same split (wide-does> lane, engine built from line tip ac12675c and the release engine 0acb0a5f alike): bin/hb --load test/compiler/native-rename-rows.f standalone fails asserts 65/66/68 (expected E-NELAB-BUNDLE -8519, got 0 - the C-BOX-OPEN/C-BOX-DEF: refusals) while the gated compiler-native-rename-rows suite passes; fix both files by the same rule.
