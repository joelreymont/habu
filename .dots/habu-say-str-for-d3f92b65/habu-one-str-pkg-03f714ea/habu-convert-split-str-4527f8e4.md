---
title: Convert split-str batch plain-20
status: active
priority: 2
issue-type: task
created-at: "2026-11-24T23:55:52.910419+01:00"
---

Problem: the 25 files of batch plain-20 (/tmp/jerry-p1/unmark/b-plain-20.txt) carry the split-str marker and fail unmarked. Acceptance: they carry no marker, pass check.f and their suites; a definition needing a public row change or a vector is reported, not converted. Depends: none. Claim: agent=jerry workspace=.jj-ws.noindex/jerry-unm-plain-20.
