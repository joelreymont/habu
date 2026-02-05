---
title: Stabilize reader repro tests
status: closed
priority: 2
issue-type: task
created-at: "\"\\\"2026-02-05T23:25:28.131298+01:00\\\"\""
closed-at: "2026-02-05T23:25:43.813686+01:00"
close-reason: Stabilized reader repro assertions via Repl and stdlib
---

Context: /Users/joel/Work/habu/src/tests/integration.zig:3070-3150; cause: post-fix reader repro assertions were brittle without stdlib context; fix: run reader repros through Repl+stdlib and use package-qualified *read-suppress* binding to avoid symbol/package skew; deps: habu-fix-reader-printer-d2f6a737; verification: zig build test.
