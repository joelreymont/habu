---
title: Reject checker statements in programs
status: closed
priority: 2
issue-type: task
created-at: "2026-11-18T16:11:36.421971+01:00"
closed-at: "2026-11-18T17:39:37.340064+01:00"
---

Problem: two checker statements still pass in a program. `EPRIM: cs-extra PE-N EPRIM;` passes tools/check.f -- --verify-only, rc 0, though a run rejects it E-UNDEFINED: src/habu/verify-source.f:3784-3785 records a primitive row in any file. `undefine IMK-NDICT0` runs, rc 0, and removes the marker src/core/prefix-boundary.f FIRST-RECORD finds by name in the global wordlist (src/core/util.f:8). Acceptance: both are rejected E-UNDEFINED under tools/check.f and when run; `undefine drop` in a program still runs. Depends: none. Claim: agent=jerry workspace=.jj-ws.noindex/jerry-cs.
