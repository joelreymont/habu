---
title: Give map.f a package and settle render.f
status: closed
priority: 2
issue-type: task
created-at: "\"\\\"2026-09-30T16:51:16.775051+02:00\\\"\""
closed-at: "2026-09-30T17:25:23.938377+02:00"
close-reason: "lib/map.f is package MAP with nine public words whose typed effects are unchanged and everything else private; lib/map-test.f, examples/file-map.f and docs/stdlib.md use it. lib/render.f is deleted: tools/perf-map-core.f uses the lib/string.f caller-owned builder over its own 16 KiB buffer. Owning rows rc 0 on an engine built from the tree (lib/map-test.f, tools/perf-map-test.f, tools/examples-test.f, test/compiler/native-tail.f); perf-map output byte-identical on 732 lines. One difference: a rewritten line past 16 KiB throws E-STR-CAPACITY (-2201) where it threw -6210; the stdin driver caps a line at 4096 bytes. Independent review: ACCEPT."
---

Problem: lib/map.f (289 lines) declares no package, against the rule that every module has a real package; lib/render.f (39 lines, package RENDER, no test) is required only by tools/perf-map-core.f. Acceptance: map.f's words live in a real package with typed public effects and every caller (lib/map-test.f, tools/examples-test.f, test/compiler/native-tail.f, docs/stdlib.md) uses it; render.f is replaced by an existing library if one expresses it (lib/byte-buffer.f, lib/fmt.f), else it moves beside its one tool; no behavior change. Files: lib/map.f, lib/map-test.f, its callers, lib/render.f, tools/perf-map-core.f, docs/stdlib.md. Verify: the suites that own those files. Depends: none. Ownership: those files. Claim: agent=kestrel workspace=.jj-ws/r4-pkg.
