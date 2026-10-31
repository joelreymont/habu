---
title: Add a logging library and log MODEL failures
status: active
priority: 1
issue-type: task
created-at: "2026-10-31T18:17:11.473038+02:00"
---

Joel 2026-10-31: "also, make sure model library logs failures, errors, etc. dumping the textual reason. we should have a logging library in habu regardless". Habu has no logging library; crew.f, vocabulary.f, ERROR-TEXT:TYPE and the pending dlopen report each write their own stderr line, and MODEL keeps a failure's text in the call (ERROR$, HTTP-STATUS, FINISH-REASON$) where nothing writes it. Design: Fable Plan (brief carl-dots/.out/carl-file/brief-log-design.md); public names go to Joel. Acceptance: the library on master, MODEL logging every failure with its text, names Joel chose.
