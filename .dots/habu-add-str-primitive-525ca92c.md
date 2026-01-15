---
title: Add string>= primitive
status: open
priority: 2
issue-type: task
created-at: "2026-01-15T20:38:21.417696+02:00"
---

src/runtime/primitives/string.zig: Add string_ge function. Return t if s1 >= s2. Dependencies: none. Verify: (string>= "b" "a") => t
