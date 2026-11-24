---
title: Convert split-str batch plain-14
status: closed
priority: 2
issue-type: task
created-at: "2026-11-24T23:30:08.401445+01:00"
closed-at: "2026-11-24T23:40:04.621794+01:00"
---

Problem: the 25 files of batch plain-14 (/tmp/jerry-p1/unmark/b-plain-14.txt) carry the split-str marker and fail unmarked. Acceptance: they carry no marker, pass check.f and their suites; a definition needing a public row change or a vector is reported, not converted. Depends: none. Claim: agent=jerry workspace=.jj-ws.noindex/jerry-unm-plain-14.
