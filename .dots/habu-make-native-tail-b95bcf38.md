---
title: Make native-tail.f green standalone
status: open
priority: 3
issue-type: task
created-at: "2026-09-22T06:42:51.076741+03:00"
---

Problem (measured on the integrated engine 6be58e41 at d4673e31): bin/hb --load test/compiler/native-tail.f alone under env -i with a scratch HOME prints 'assert: expected false got true', '14' and 'test: failures' (14 TFAILs), while the same suite is green inside test/run.f (PASS: compiler-native-tail), and the x64-resid lane measured the standalone log byte-identical before and after its change: a load-context artifact of the file, the family the 'Load after' sweep closed for lib/ and test/ (225a093a). Acceptance: identify what run.f's load order supplies that the standalone load does not (a require, a tier selection, a fixture registered elsewhere), have native-tail.f require or select it itself, standalone green under env -i with a scratch HOME, run.f still green. Files: test/compiler/native-tail.f, tools/codegen-tail-probe.f. Verify: the suite alone; test/run.f. Depends: none. Ownership: hazel (native tests). Claim: unassigned.
