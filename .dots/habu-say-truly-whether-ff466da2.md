---
title: Say truly whether a DYNAMIC-BUFFER is released before a save
status: open
priority: 3
issue-type: task
created-at: "2026-11-10T22:48:08.076708+02:00"
---

Problem: docs/forth-card.md:345 tells the reader to release a DYNAMIC-BUFFER before an image save; src/core/dynamic-storage.f:5-8 says release zeroes the record and that a save or a file's object holds every record released and the registry empty even while the process keeps them, which seems to make the instruction unnecessary (not verified by a run). Acceptance: the card's line says what a program must do, shown by a program that saves an image with a live DYNAMIC-BUFFER and what the restored image holds, or the dot closes with the evidence that the line is true. Files: docs/forth-card.md, and docs/forth.md where it says the same. Verify: first, that program; then read the changed lines against it. Depends: none. Ownership: unassigned. Claim: unassigned. Consumers: met by none.
