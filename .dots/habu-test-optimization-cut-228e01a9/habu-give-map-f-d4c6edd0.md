---
title: Give map.f a package and settle render.f
status: active
priority: 2
issue-type: task
created-at: "\"2026-09-30T16:51:16.775051+02:00\""
---

Problem: lib/map.f (289 lines) declares no package, against the rule that every module has a real package; lib/render.f (39 lines, package RENDER, no test) is required only by tools/perf-map-core.f. Acceptance: map.f's words live in a real package with typed public effects and every caller (lib/map-test.f, tools/examples-test.f, test/compiler/native-tail.f, docs/stdlib.md) uses it; render.f is replaced by an existing library if one expresses it (lib/byte-buffer.f, lib/fmt.f), else it moves beside its one tool; no behavior change. Files: lib/map.f, lib/map-test.f, its callers, lib/render.f, tools/perf-map-core.f, docs/stdlib.md. Verify: the suites that own those files. Depends: none. Ownership: those files. Claim: agent=kestrel workspace=.jj-ws/r4-pkg.
