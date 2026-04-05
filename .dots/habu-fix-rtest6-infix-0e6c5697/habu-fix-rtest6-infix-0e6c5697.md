---
title: Fix rtest6 infix operator state
status: active
priority: 2
issue-type: task
created-at: "\"2026-04-04T21:02:33.701470+02:00\""
---

PLAN.md phase 3 test-batch authority and tools/maxima-rtest.lisp canonical path. Root cause surfaced after pathname cutover: canonical rtest6 now runs and dies at ../maxima/tests/rtest6.mac:110 with 'infixie is not an infix operator'. Investigate operator declaration/storage/parser visibility so dynamic infix definitions are seen by subsequent reads generically. Files likely: ../maxima/src/nparse.lisp, operator declaration paths in Maxima core, and reader/parser/operator state in Habu. Acceptance: canonical tools/maxima-rtest.lisp rtest6 advances past line 110 without syntax error.
