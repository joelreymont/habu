---
title: Define exact native source view
status: open
priority: 2
issue-type: task
created-at: "2026-09-29T14:00:46.309740+02:00"
---

Design the source transaction that makes prefix discovery, cache key and Habu included/required compilation consume the same owned bytes and path/root identity. Current EC re-reads source independently and include.f reopens it, so membership checks alone are insufficient. Specify immutable span ownership, resolver behavior, repeated include and require semantics, missing/new dependency refusal, and a deterministic edited-source acceptance case. Read-only design before cache publication; no metadata heuristic.
