---
title: Leave a catch over a body left to the run
status: active
priority: 2
issue-type: task
created-at: "2026-11-25T19:04:23.279834+01:00"
---

Problem: src/core/check-walk.f:1606 records a body left to the run with no throw, so a caught quotation that calls it is judged as never throwing: `: GEN ( -- ) s" : MADE ( n -- n ) dup 0 > if drop 7 throw then ;" evaluate-closed ;` / `GEN` / `: LATE ( n -- n ) MADE ;` / `: U ( n -- n ) [: LATE ;] catch drop ;` loads rc 70 E-STALE-READ and checks rc 0 with no warning on U. Recording CTL-THROW would reject programs the load accepts, so such a catch is left to the run instead. Acceptance: the check leaves U to the run, with its warning. Depends: none. Claim: agent=jerry workspace=.jj-ws.noindex/jerry-ck-left.
