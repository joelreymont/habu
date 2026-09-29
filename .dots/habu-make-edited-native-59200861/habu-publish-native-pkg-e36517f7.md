---
title: Publish native package code and records
status: active
priority: 2
issue-type: task
created-at: "\"2026-09-29T17:49:16.056569+02:00\""
---

Implement one internal native-unit-publish primitive that appends relocated code and complete 48-byte dictionary rows, advances CP and NDICT, maintains HIDX and relocation maps, and records native origin. Acceptance: compile a new host and exercise the explicit NBR E2E import path; reject invalid spans before mutation.
