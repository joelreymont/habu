---
title: Find whether grown token buffers reach a saved image
status: active
priority: 2
issue-type: task
created-at: "2026-11-15T10:29:24.437509+01:00"
---

Problem: src/core/checker.f:4930-4984 keeps the checker's token buffers in PERSISTED-PTR-VARIABLEs (FAILTK-P, TKF-P, NMB-P, SDEC-P) with a plain capacity cell TOKBUF-CAP-U, and TOKBUF-GROW moves them to pages map-anon gives (TOKBUF-ALLOC). A check of a token longer than 4096 bytes (test/fixtures/build-items/long-type.f) grows them. If the process that grew them is then saved as an image, the save either refuses the cell (docs/forth-card.md:437-440: a pointer into memory the build mapped is refused at save) or writes an address the restored image never mapped. Whether any save route meets the grown cells is not known: a build checks the program with the host's checker. Acceptance: a reproducer through each image a build makes (with and without --repl), after a long-token definition, shows what the save and the saved image do, and names the cause; a defect it finds becomes the fix's own dot. Depends: none. Claim: agent=worker workspace=.jj-ws.noindex/carl-tokhunt.
