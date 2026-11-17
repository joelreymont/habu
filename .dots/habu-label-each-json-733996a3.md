---
title: Label each JSON reader test case
status: active
priority: 2
issue-type: task
created-at: "2026-11-17T17:05:50.438144+01:00"
---

Problem: src/lib/json-read-test.f sets its last T-LABEL at :1193 ('reader cannot be closed twice'), so every later failing case (the JRT-BAD-* runs ~1290-1300 and the groups after) reports under that label: a short-destination failure printed as the close-twice case. Acceptance: each case after :1193 states its own label; a deliberately broken later assertion prints its own case. Depends: none. Claim: agent=carl workspace=.jj-ws.noindex/carl-jlabels.
