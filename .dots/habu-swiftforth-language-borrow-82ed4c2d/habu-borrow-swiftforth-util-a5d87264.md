---
title: Borrow SwiftForth utility sections
status: open
priority: 2
issue-type: task
created-at: "2026-06-28T00:10:54.340148+02:00"
---

Files: local SwiftForth Reference Manual PDF in /home/user, docs/forth.md, docs/stdlib.md, lib/*.f/tools/*.f as selected. Root cause: the requested SwiftForth sections (timing functions, string buffers, string data structures, linked lists, switches, execution vectors, exceptions/error handling) have not been systematically mapped against Habu's existing stdlib/tooling. Fix: extract each section from the manual, compare against current Habu facilities, implement the small typed words that are clearly useful and non-legacy, and dot any larger capability with syntax, semantics, files, tests, and why. Why: borrowing should be deliberate and documented, not a vague feature grab.
