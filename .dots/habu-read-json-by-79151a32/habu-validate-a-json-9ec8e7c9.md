---
title: Validate a JSON value against a schema
status: closed
priority: 2
issue-type: task
created-at: "2026-11-24T17:22:27.429881+01:00"
closed-at: "2026-11-24T19:40:35.258876+01:00"
---

Problem: JSON-SCHEMA:VALID? takes text and walks it with a cursor. Acceptance: VALID? ( JSON:value schema ^VEC:vector<u8> -- bool ) walks by JSON:WALK with its own frame stack, violation texts unchanged, a 100000-deep value as its test. Depends: compile lane. Claim: unassigned.
