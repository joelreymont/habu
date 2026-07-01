---
title: Reject prose dot dependency markers
status: closed
priority: 1
issue-type: task
created-at: "\"\\\"2026-07-01T22:30:05.683747+02:00\\\"\""
closed-at: "2026-07-01T22:40:22.944926+02:00"
close-reason: "completed: dot-dep-lint now rejects capitalized prose dependency markers without YAML blocker items; focused tools/dot-dep-lint-test.f, live dot-dep-lint, lint-tools, and typed-local diff lint passed"
---

File: PLAN.md:579; cause: dot dependency prose such as Deps or Needs does not enforce ordering and lets stale blockers drift; fix: extend tools/dot-dep-lint.f and tests to reject prose dependency markers unless matching YAML blocks front matter exists; deps: none; verification: focused dot-dep-lint test with prose-only dependency fails, YAML blocks passes, and the tool-lints or owning gate slice includes the check.
