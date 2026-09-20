---
title: Register native-colon in the gate
status: active
priority: 2
issue-type: task
created-at: "2026-09-16T16:57:00.784251+03:00"
---

Problem: test/compiler/native-colon.f appears in no SUITE in test/gate-stdlib-cases.f and nothing references it; it runs green standalone (17 cases) but the gate never runs it (sweep lane, 2026-09-16). Acceptance: registered with the other native compiler suites (aot-mode.f prefix), green in the gate; a lint or a gate self-test that lists test files not registered anywhere, with an allowlist of children and libs, so the next orphan is a red. Files: test/gate-stdlib-cases.f, tools/ (the orphan check). Verify: test/run.f. Depends: none. Ownership: gate registration. Claim: alder, `.jj-ws/alder-native-colon`, based on integrated `4949a71c`.
