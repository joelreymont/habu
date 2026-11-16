---
title: Load a --load path that holds a double quote
status: open
priority: 2
issue-type: task
created-at: "2026-11-16T13:36:18.784685+01:00"
---

Problem: src/habu/main.f STREAM writes each --load file as source text, `s" <path>" script-required` (ROW), so a path holding a double quote ends the literal early and the rest runs as words (E-UNDEFINED at a line the user never wrote). Acceptance: `hb --load` of a file whose path holds a double quote, a backslash or a LF loads it; a test in test/main-routing.f fails first. Depends: none. Claim: unassigned.
