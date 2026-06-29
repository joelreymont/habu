---
title: Test scheduler for semantic work
status: open
priority: 1
issue-type: task
created-at: "2026-06-29T22:22:47.746366+02:00"
---

Problem: test/gate-stdlib-inline-lib.f has one synchronous SUITE-INLINE-WORK hook, so adding check-repair/doc/typed-local semantics serialized zed's tool test slice (~56s) instead of cutting the critical path. Fix: replace the hook with checked test metadata carrying subject/runner/deps; host-source semantic tests execute through the resident controller as independent scheduled tests, while CLI tests stay child/candidate boundaries. Files: test/run.f, test/gate-stdlib-lib.f, test/gate-stdlib-inline-lib.f, test/gate-runner-entry.f, docs/gate.md. Verify: zed hot test suite shows test timings for each semantic group and no monolithic inline critical path.
