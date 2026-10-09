---
title: Forget a definition a check hook rejects
status: active
priority: 1
issue-type: task
created-at: "2026-10-09T22:14:02.812162+03:00"
---

Problem: a check hook that answers 0 for a definition with no signature (pk1-pk6 install LOWER-CERT-HOOK:HOOK, then such a hook) makes the load drop a definition the checker has already recorded (src/core/checker.f:12450-12454; src/habu/habu2.f EM-COMPILE-PUBLISH-HOOKED, its rejected arm). ndict does not change, so CK-PEND-SYM (checker.f:12465-12469), which tests only ndict, and CK-PENDING-SYM (checker.f:12486-12493) still name the dropped W. Later words are checked against it while the engine binds another W: pk5 certifies `T ( -- n n )` against the dropped W, compiles the global W ( -- n ), prints 1, then dies `interpret stack underdepth` rc 70; pk6 is mistyped the same way. With W shadowed or ambiguous, LIVE-BIND (checker.f:12654-12669) leaves BIND-REC null: native compiles the engine's lookup (pk1-pk4 rc 0) and the Gforth host dies `binding kind` or `tick target` rc 76 (src/host/gforth/codegen.fs CALLEE, H-TICK). Probes: /Users/joel/.cache/tmp/heron-arm64/evidence/gfcodegen/probes/i8/pk1.f-pk6.f.
Acceptance: rejecting a recorded definition leaves no pending window or record for it, so every later word is checked against the definition the engine binds. pk1-pk6 join the native suite with the corrected outcomes and join test/gforth/cases/ matching native. If codegen.fs CALLEE's `binding kind` and H-TICK's `tick target` deaths are then unreachable, they go.
Files: src/core/checker.f, src/habu/habu2.f, the native tests of the check-hook path a search finds, src/host/gforth/codegen.fs, test/gforth/cases/.
Verify: rebuild bin/hb per docs/gate.md; `bin/hb --load test/run.f`; two-generation build converges; `HB_TMP=$PWD/build/tmp bin/hb --load test/gforth/host-test.f`.
Depends: none. Ownership: the files above. Worker: worker-max. Claim: agent=worker-max (lead carl) workspace=.jj-ws/carl-forget.
