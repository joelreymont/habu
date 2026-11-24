---
title: "Find whether an index into hb's records holds across builds"
status: active
priority: 2
issue-type: task
created-at: "2026-11-24T19:40:27.154523+01:00"
---

Problem: src/habu/linker.f HB-REC (about :4185 at the damage commit mlmvuxml) bounds an object's reference to one of hb's records by FIRST-REC, ndict@ at MOVE-IN: what the building process loaded before the build. The object folder key (ENGINE-ID) does not cover that, so an object written by a process that loaded more or less before the build may name by index a record another process does not hold, or a different one; a warm build could then fail or link a wrong record where a cold one would not. Inferred from the code by the damage lane, not reproduced. Acceptance: a reproducer (two processes that load differently before one build, cold then warm) shows the difference and the cause is fixed there, or the code shows every build process holds the same records below FIRST-REC. Depends: none. Claim: agent=jerry workspace=.jj-ws.noindex/jerry-hbrec.
