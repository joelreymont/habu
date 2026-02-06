---
title: Fix ANSI UnexpectedToken regression
status: closed
priority: 1
issue-type: task
created-at: "\"2026-02-06T04:04:01.281489+01:00\""
closed-at: "2026-02-06T04:19:30.044044+01:00"
close-reason: "Resolved UnexpectedToken regression (next: TypeMismatch)"
---

Context: /Users/joel/Work/habu/docs/ansi/raw/habu-latest.log and /tmp/habu-ansi/ansi-test/init.lsp expr 2; cause: reader fails with UnexpectedToken while parsing nested feature conditionals in init.lsp; fix: isolate parser conditional skipping bug in src/reader/parser.zig, add regression test, and rerun ansi latest; deps: habu-check-regression-and-f81466fe; verification: habu regression report has no unexpected_failures.
