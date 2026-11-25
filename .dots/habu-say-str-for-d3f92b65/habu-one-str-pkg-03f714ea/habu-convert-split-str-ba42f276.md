---
title: Convert split-str batches plain-34, 35 and 37
status: active
priority: 2
issue-type: task
created-at: "2026-11-25T11:35:40.866522+01:00"
---

Problem: 8 tool and test files (/tmp/jerry-p1/unmark/b-plain-34-37.txt) still carry the split-str marker; Joel 2026-11-23: the marker should be removed. Acceptance: no split-str line in them; each passes check.f; their suites pass. Depends: none. Claim: agent=jerry workspace=.jj-ws.noindex/jerry-unm-plain-34-37.
