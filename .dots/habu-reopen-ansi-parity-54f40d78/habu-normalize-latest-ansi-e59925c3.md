---
title: Normalize latest ANSI logs
status: closed
priority: 1
issue-type: task
created-at: "\"2026-02-06T03:53:17.844768+01:00\""
closed-at: "2026-02-06T03:55:13.930787+01:00"
close-reason: Parsed latest ANSI raw logs into normalized sbcl/habu JSON
---

Context: /Users/joel/Work/habu/tools/ansi/parse_results.py and docs/ansi/results; cause: no latest normalized json; fix: parse latest raw logs into docs/ansi/results/sbcl-latest.json and docs/ansi/results/habu-latest.json; deps: habu-run-latest-ansi-92e967c6; verification: both json files parse and contain counts.
