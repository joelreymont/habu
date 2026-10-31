---
title: Check the primitive entry words
status: active
priority: 2
issue-type: task
created-at: "2026-10-31T13:36:52.098823+02:00"
---

Problem: after habu-let-signatures-name-5931014b a primitive's tick can be declared xt and cast privately to n, so the seven tick-only TRUSTED: words convert. Acceptance: elaborate.f and t0.f keep no TYPE-ENTRY/XT-STORE-ENTRY/EXECUTE-ENTRY (uses inline ['] w XT>N with a private CAST: XT>N ( xt -- n ) per package); DOES-PATCH-ENTRY stays TRUSTED: as one public copy in NDICT (dict.f) both call; select-x64.f THROW-ENTRY gone, EMIT-DIV inline; trap.f ROUTINE is : with the same effect; comments citing deleted words corrected. EXECUTE-ENTRY stays TRUSTED: only if ['] execute is refused in a checked body (record the refusal). Files: src/compiler/native/elaborate.f, src/habu/t0.f, src/compiler/native/dict.f, src/compiler/native/select-x64.f, src/compiler/native/trap.f, test/compiler/x64-emit-fixture.f. Verify: engine build (state which HOST), x86-64 cross-build, SUITEs tier, compiler-x64-emit, compiler-native-match, compiler-native-match-aot, the runtime-regression row. Depends: habu-let-signatures-name-5931014b. Ownership: those files. Brief /tmp/jerry-big/tcl-tick2-brief.md. Claim: agent=jerry worker workspace=.jj-ws.noindex/jerry-tick2
