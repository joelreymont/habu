---
title: Let the file-set walker thread caller state
status: open
priority: 2
issue-type: task
created-at: "2026-08-11T10:53:07.294118+02:00"
---

Found in the fold-handle landing (95cdc199): TR-FILES-WALK (test/run-files.f) fixes its callback contract at ( ptr u8 n -- ), so a caller folding state across the walk cannot thread it - test/run-lib.f parks its CONTENT-KEY:fold in a typed cell (documented at the site, the one place the handle is not on the stack). Structural fix: give the walker a state-threading variant (callback ( state ptr u8 n -- state )) and migrate run-lib.f's phase-key derivation onto it, deleting the parked cell. Check other TR-FILES-WALK callers for the same parked-state shape while there. Files: test/run-files.f, test/run-lib.f. Depends: none.
