---
title: Report why a load fails with rc 78 and no message
status: open
priority: 2
issue-type: task
created-at: "2026-10-30T22:40:57.493163+02:00"
---

Census C (carl-b1/.out/census/census-C.md): a case file whose top level runs `' depth HS !` (HS a typed xt cell) made the builder-session load fail with rc=78 and printed nothing (/tmp/carl-b1/c-executed-closure-unknown-q). A failure without a message is a defect: reduce it, find which layer exits silently, and make it name the cause.
