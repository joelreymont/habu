---
title: Convert split-str batch plain-10
status: active
priority: 2
issue-type: task
created-at: "2026-11-24T23:13:15.080199+01:00"
---

Problem: the 5 files of batch plain-10 (/tmp/jerry-p1/unmark/b-plain-10.txt) carry the split-str marker and fail unmarked. Acceptance: they carry no marker, pass check.f and their suites; a definition needing a public row change or a vector is reported, not converted. Depends: none. Claim: agent=jerry workspace=.jj-ws.noindex/jerry-unm-plain-10.
