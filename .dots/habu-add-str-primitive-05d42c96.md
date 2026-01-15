---
title: Add string<= primitive
status: open
priority: 2
issue-type: task
created-at: "2026-01-15T20:38:17.398667+02:00"
---

src/runtime/primitives/string.zig: Add string_le function. Return t if s1 <= s2. Dependencies: none. Verify: (string<= "a" "a") => t
