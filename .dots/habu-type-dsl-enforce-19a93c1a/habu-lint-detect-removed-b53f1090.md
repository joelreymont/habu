---
title: "Lint: detect removed type tokens"
status: open
priority: 1
issue-type: task
created-at: "2026-07-13T17:19:43.434595+02:00"
blocks:
  - habu-delete-legacy-type-36040d18
---

Own a checked Habu token-aware lint and focused tests that rejects every removed type definer/delimiter outside the allowlisted rejection fixtures and migration history. Parse tokens rather than raw substrings, report exact file/line/token, and fail closed on malformed source.
