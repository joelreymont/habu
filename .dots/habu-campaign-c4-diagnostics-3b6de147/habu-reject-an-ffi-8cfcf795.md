---
title: Reject an FFI group open across an include in the verifier
status: open
priority: 2
issue-type: task
created-at: "2026-11-15T09:25:16.153230+01:00"
---

Problem: a FUNCTION: group opened in one file and a FUNCTION: or BEGIN-STRUCTURE in a file it includes before ;FUNCTION (/tmp/jerry-h8c/a.f, a2.f): the load places the rejection at the `<input>` script-required line; tools/check.f places the nested FUNCTION: case at the inner opener labelled "opened here"; --verify-only passes the BEGIN-STRUCTURE case rc 0. A rejection places a token only in its one armed source or the top frame (src/core/rejection.f PLACE-IN, src/core/include.f PUBLISH-FRAME), never in an including file, and src/habu/verify-source.f FFI-OPEN-HERE? answers false for an opener outside the scanned source. Design brief: /tmp/jerry-8cfcf795-design.md. Acceptance: the three readers print one and the same record, rc 70. Depends: none. Claim: unassigned.
