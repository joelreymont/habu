---
title: "Test tier 0's +loop, j, r@, leave, until and again and a NaN operand through a Wasm build"
status: active
priority: 2
issue-type: task
created-at: "2026-11-10T19:22:00.003449+02:00"
---

Problem (feature review A, F1 and F5): no test in the tree reaches src/habu/t0-wasm.f LOOP-STEP and ENTRY-EQ (+loop), LOOP-J, RFETCH, LEAVE, or begin-until and begin-again through a Wasm build, and link-routines.f FARITH-BODY's NaN rule (CANON, PASS,) holds in test/wasm/hb-build.f's f.f with both words removed. Lead's probe on the stack's engine: STEP3 DOWN NONE OUTER PEEK QUIT5 UNTIL3 AGAIN4 (/tmp/jerry-pa/cf.f) print 18 15 0 12 14 5 3 4 natively and from the module. Acceptance: test/wasm/hb-build.f holds those forms and a NaN operand case, each compared with what the host prints. Files: test/wasm/hb-build.f. Verify: the test ends test: ok. Depends: none. Ownership: that file. Claim: agent=jerry workspace=.jj-ws.noindex/jerry-r5.
