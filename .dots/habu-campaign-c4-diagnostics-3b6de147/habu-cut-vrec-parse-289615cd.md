---
title: "Cut VREC-PARSE-FIELDS's dead branch and say reject"
status: closed
priority: 2
issue-type: task
created-at: "2026-11-15T12:50:22.220481+01:00"
closed-at: "2026-11-16T04:07:36.507675+01:00"
---

Problem: checker.f VREC-PARSE-FIELDS has an SGBAD branch no input reaches; VREC-TYPE-REFUSE is a new name that says refuse. Acceptance: the branch is gone (or a reaching input is pinned); the word says reject. Depends: none. Claim: agent=jerry workspace=.jj-ws.noindex/jerry-c4m2 (in the integration stack, closes at landing).
