---
title: Publish native package code and records
status: closed
priority: 2
issue-type: task
created-at: "\"\\\"2026-09-29T17:49:16.056569+02:00\\\"\""
closed-at: "2026-09-30T12:09:04.135397+02:00"
close-reason: Internal native-unit-publish installs validated relocated code and complete dictionary rows through engine-owned append, HIDX and origin updates; importer preflights spans/references before publication. Fresh real NBR export/import hit passed with exact hb/names cold parity, stale-source and checker refusals. Astra PASS;499/499 current-source gate and zero-byte convergence passed.
---

Implement one internal native-unit-publish primitive that appends relocated code and complete 48-byte dictionary rows, advances CP and NDICT, maintains HIDX and relocation maps, and records native origin. Acceptance: compile a new host and exercise the explicit NBR E2E import path; reject invalid spans before mutation.
