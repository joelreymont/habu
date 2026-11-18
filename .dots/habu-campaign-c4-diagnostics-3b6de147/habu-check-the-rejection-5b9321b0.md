---
title: Check the rejection builder across tasks
status: closed
priority: 2
issue-type: task
created-at: "2026-11-18T07:27:44.208282+01:00"
closed-at: "2026-11-18T08:48:12.837173+01:00"
---

Problem: src/core/rejection.f's builder state (TEXT, PART, DEST, the place cells, HELD, SAVED, :14-60, :228-279, :381-409, :1140-1143) is process-wide while the carry stamp is per thread (src/habu/layout.f STAMP-CELL): four tasks taking refused reserves at once interleave records, lose them and send record bytes to stdout (/tmp/jerry-recrace/race.f). Acceptance: each task builds, saves and sends its records in its own builder state; race.f's program is a test that fails on master and passes. Depends: none. Claim: agent=jerry workspace=.jj-ws.noindex/jerry-recfix.
