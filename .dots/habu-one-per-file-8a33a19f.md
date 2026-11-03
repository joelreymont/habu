---
title: One per-file report from the build cache
status: open
priority: 2
issue-type: task
created-at: "2026-11-03T10:19:39.529146+02:00"
---

Problem (Fable objk review F6, 2026-11-02): tools/native-definition-cache.f keeps two per-file build reports. HABU_NATIVE_CACHE_DIAG=1 turns on NUNIT-DEP:DIAG? (:2010), read by the native-file e2e tests (test/native-file-types-e2e.f:184, native-file-auto-e2e.f:34, native-file-frame-e2e.f:82, native-file-app-lib.f:73) and about 10 MB of stderr per engine build (test/prefix-provided-unfilled.f:60). HABU_BUILD_FILES=<path> (:141-151 LOAD-ROWS/LOAD-TEXT, the row recorder ~:1035-1046, the writer ~:1961-1979, about 70 lines) writes '<hit> <live> <path>' per file for one caller, test/compiler/native-build-edit-e2e.f (:376, ROW-JUDGE :468). Two switches and two formats report the same fact, which file was reused. Acceptance: one report of which files a build reused, written by one recorder, read by every test that needs it; the other switch, its buffers and its writer deleted; native-build-edit-e2e.f and the native-file e2e rows pass unchanged in what they assert. Decide at dispatch which survives: the compact rows are what an engine build can afford (DIAG's 10 MB stderr is the cost to beat). An env name counts as a public interface: removing one needs no approval only if no client outside this repo sets it (check maki, zen, tender, etch). Files: tools/native-definition-cache.f, tools/native-unit-dependency.f, the tests named above. Verify: those tests. Depends: none. Ownership: tools/native-definition-cache.f. Claim: unassigned.
