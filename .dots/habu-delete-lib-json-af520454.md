---
title: Delete lib/json-read.f (zero consumers, ~950 lines)
status: open
priority: 2
issue-type: task
created-at: "2026-07-18T14:15:40.488548+02:00"
---

Depth review 2026-07-18: 570 lines + 342-line test, zero production consumers (require-grep across src/lib/maki/test/tools: only own test/fixtures); not in docs/stdlib.md manifest (json-write is, json-read never); all production JSON is write-only. Delete with its E-JR-* rows in lib/errors.f, or name the intended consumer (artifact-db ingest?) in a dot. Risk: none in-repo.
