---
title: "Place a replayed definition's rejection in its source"
status: active
priority: 2
issue-type: task
created-at: "2026-11-16T17:10:36.900398+01:00"
---

Problem: a replayed definition that finds no dictionary record (src/core/checker.f ROOM) is rejected at <input>:1:1 showing the replay buffer's text as line 1, and names the word folded (`ra` for `RA`); reproducer /tmp/carl-vw/case.f on the carl-vw build (test/dictionary-full-test.f REPLAY-PARKED expects that output). Acceptance: the rejection names the word as written and places it in the source that holds the replayed text, as a rejection in evaluated text is placed; the test expects that. Depends: none. Claim: agent=worker (hunt) workspace=.jj-ws.noindex/carl-vw (read only).
