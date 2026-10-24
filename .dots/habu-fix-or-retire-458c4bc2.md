---
title: Register test/aot-native-does.f on the x86-64 gate
status: open
priority: 3
issue-type: task
created-at: "2026-10-24T02:34:21.830319+03:00"
---
test/aot-native-does.f is an x86-64-host test of the native capture path (AOT-CAPTURE:CAPTURE-NATIVE with SHADOW-PRE-REACH / SHADOW-LIVE?, then SHADOW-CAPTURE; tools/native-build-core.f routes x86-host shadows there). It is in no gate suite. On an arm64 host it fails rc 67 'ncomp: cannot compile MAKER', as the registered x86-only test/aot-native-merge.f does, so that failure is no evidence it is stale. test/aot-shadow-capture.f (ARM, TARGET-CAPTURE) does not hold its coverage: on the native path nothing registered asserts that a childless definer keeps its ;does clause record and shadow row through WRITE and READ (aot-capture.f ACAP-NATIVE-ROOTS companion loop, aot-shadow.f SH-NATIVE-DOES). Its assertion that a private definer reached only through a public wrapper ships no record matches master (aot-capture.f ACAP-NAMED? and the comments at ~943 and ~2170); the intel bookmark's d8dca0f8 'Capture x86 shadows through one entry' deletes CAPTURE-NATIVE and rewrites this test to keep MAKER privately, so do not delete it on master (delete/modify conflict at the Intel merge).

Acceptance: on an x86-64 host, run it against a master engine; repair what fails and register it as a SUITE row in test/gate-x64-cases.f (or fold its MAKER;does assertions into test/aot-native-merge.f, whose window already has MAKER and USE-MAKER), asserting master's rule. Blocked: no x86-64 host reachable from jerry's arm64 session (2026-10-24).
