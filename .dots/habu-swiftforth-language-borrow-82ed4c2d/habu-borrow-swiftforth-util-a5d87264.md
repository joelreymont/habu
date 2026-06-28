---
title: Borrow SwiftForth utility sections
status: closed
priority: 2
issue-type: task
created-at: "\"2026-06-28T00:10:54.340148+02:00\""
closed-at: "2026-06-28T08:27:10.539503+02:00"
close-reason: "implemented checked BUFFER:/BUF-* utilities, documented stdlib surface, validated with focused checks and full native gate; split checked exec/switch/list work into child dots"
---

Files: local SwiftForth Reference Manual PDF in /home/user, docs/forth.md, docs/stdlib.md, lib/*.f/tools/*.f as selected. Root cause: the requested SwiftForth sections (timing functions, string buffers, string data structures, linked lists, switches, execution vectors, exceptions/error handling) have not been systematically mapped against Habu's existing stdlib/tooling. Fix: extract each section from the manual, compare against current Habu facilities, implement the small typed words that are clearly useful and non-legacy, and dot any larger capability with syntax, semantics, files, tests, and why. Why: borrowing should be deliberate and documented, not a vague feature grab.

Checkpoint 2026-06-28: extracted the requested SwiftForth manual sections from
the local PDF. Habu already has `TIME-EPOCH-SECONDS`/`TIME-MONO-NS`, checked UTC
date formatting, `SB-*`, and `catch`/`throw`; SwiftForth's unchecked
`PLACE`/`APPEND`/`ZPLACE`/`ZAPPEND` were adapted as checked capacity-bearing
`BUFFER:`/`BUF-*` string helpers instead of copied raw. Larger SwiftForth
mechanisms that depend on stored xt effects or relocatable linked lists were
split into child dots: checked execution vectors, checked switch DSL, and
linked-list defining-word assessment.
