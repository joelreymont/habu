---
title: "Fold the case of does> in check.f's pre-pass"
status: closed
priority: 3
issue-type: task
created-at: "\"2026-10-03T16:39:51.758060+03:00\""
closed-at: "2026-10-14T19:33:10.273331+03:00"
close-reason: "Implemented in 9d895f68: the verifier pre-pass recognizes mixed-case does> in both colon and TRUSTED: definers; failing-before fixture and native-create-does check modes passed."
---

Found by lane 455 (e81287d4, c5fd9f2c): tools/check.f on test/compiler/native-create-does.f gives rc 70 E-UNDEFINED at line 35, the mixed-case DoEs> in MAKE-INERT-CELL, on dca3070a and its parent alike; the engine accepts the spelling. e77533f3 made the pre-pass's control words case-insensitive (STR=CI in WRAP-*-TOK?), but does> is not among them. Acceptance: check.f's pre-pass matches every engine control word ignoring case as the engine does, so the file checks clean; seen failing first through check.f.
