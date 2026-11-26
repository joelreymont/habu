---
title: "Rename CURL:WITH-LOOPS to CURL:WITH-LOOP"
status: open
priority: 2
issue-type: task
created-at: "2026-11-27T15:51:03.520434+01:00"
---

Problem: src/lib/net/curl.f:1876 WITH-LOOPS starts only CURL's transfer loop now that AIO has no loop task, so its name says several. Callers: Habu's curl-test.f, model-test.f, test-model-test.f; zen src/server/server.f and zen tests; SAP scripts/zen.f, scripts/read.f, test/notes-test.f; tender src/admission-store.f. Acceptance: the word is CURL:WITH-LOOP (Joel 2026-11-27, interview); every Habu caller and comment follows; each consumer's owner is told before the install. Depends: the AIO per-task queue line landing. Claim: unassigned.
