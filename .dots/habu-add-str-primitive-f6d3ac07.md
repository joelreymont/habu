---
title: Add string< primitive
status: open
priority: 2
issue-type: task
created-at: "2026-01-15T20:38:09.267666+02:00"
---

src/runtime/primitives/string.zig: Add string_lt function comparing strings lexicographically. Return t if s1 < s2, nil otherwise. Dependencies: none. Verify: (string< "a" "b") => t
