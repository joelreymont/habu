---
title: "Restore build-fixpoint-fixtures' prefix-full exits"
status: open
priority: 2
issue-type: task
created-at: "2026-10-01T18:31:14.557974+02:00"
---

Problem: tools/build-fixpoint-test.f (SUITE build-fixpoint-fixtures, test/gate-stdlib-cases.f:44) fails on ddd2f7dd: F54-56 and F62-63 'expected 74 got 70' in the source-boundary, stage2 and maker probes (PROBE :449-464, EXPECT-74 :522-525): the freshly built candidate exits 70 where 74 'hb: source prefix buffer full' is expected. It passed in gate A on d40cc36d (gate-a/gate.log:164), so a round-4 commit between d40cc36d and ddd2f7dd broke it. The installed bin/hb itself still gives 74 for a 4 MB blank --build source, so the difference is in the engine build-fixpoint builds. Found by the r4-tthrow worker ($HOME/.cache/tmp/kestrel-r4-tthrow/bft-base.log, bft-fixed.log). Acceptance: name the first failing commit (each probe with that commit's own engine and a private HB_TMP) and the wrong layer (the candidate build, the prefix-full refusal, or the test's expectation if the new exit is the right one: then say why and fix the test); tools/build-fixpoint-test.f rc 0 on the fixed tree and rc 1 on the breaking commit; a baked change gets rebuild, g1 = g2 with .names, two-generation build. Base: ddd2f7dd. Files: decided by the cause.
