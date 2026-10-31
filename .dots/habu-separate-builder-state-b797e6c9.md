---
title: Separate builder state from app state in products
status: open
priority: 2
issue-type: task
created-at: "2026-10-24T03:57:58.080972+03:00"
---

A product is the builder process saved after the app loads (APP-IMAGE:SAVE -> SNAP:PERSIST copies all DATA); builder cells are cleared by hand-kept reset lists (NDEF-CACHE CLOSE/CLEAR-SCRATCH, NUNIT-DEP:RESET, APP-BUILD RESTORE-ROOT). Any missed cell makes warm != cold. General fix: separate regions in the image layout. Workspace: none yet.
