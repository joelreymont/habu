---
title: Convert split-str batch plain-04
status: closed
priority: 2
issue-type: task
created-at: "2026-11-24T22:50:08.752341+01:00"
closed-at: "2026-11-24T23:08:17.797840+01:00"
---

Problem: the 17 files of batch plain-04 (/tmp/jerry-p1/unmark/b-plain-04.txt) carry the split-str marker and fail unmarked. Acceptance: they carry no marker, pass check.f and their suites; a definition needing a public row change or a vector is reported, not converted. Depends: none. Claim: agent=jerry workspace=.jj-ws.noindex/jerry-unm-plain-04.
