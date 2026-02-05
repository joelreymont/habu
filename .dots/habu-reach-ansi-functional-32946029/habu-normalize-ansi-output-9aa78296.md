---
title: Normalize ANSI output to JSON
status: closed
priority: 1
issue-type: task
created-at: "\"2026-02-05T22:32:05.462251+01:00\""
closed-at: "2026-02-05T22:39:30.758320+01:00"
close-reason: Added parse_results.py, docs usage, and stable runner exit-code capture.
blocks:
  - habu-add-ansi-test-05377306
---

Context: /Users/joel/Work/habu/tools/ansi/run.sh:new; cause: raw logs are not diffable at scale; fix: add /Users/joel/Work/habu/tools/ansi/parse_results.py to convert logs into normalized JSON keyed by test id; deps: habu-add-ansi-test-05377306; verification: parser emits stable JSON and total-pass/fail counts.
