---
title: Convert split-str batch plain-36
status: active
priority: 2
issue-type: task
created-at: "2026-11-25T11:04:04.983428+01:00"
---

Problem: the 15 files of /tmp/jerry-p1/unmark/b-plain-36.txt still carry the split-str marker (Joel 2026-11-23: the marker smells and should be removed). Acceptance: no split-str line in them; each checks as on master; the batch's suites pass. Depends: none. Claim: agent=jerry workspace=.jj-ws.noindex/jerry-unm-plain-36.
