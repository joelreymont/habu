---
title: SwiftForth language borrow pass
status: open
priority: 1
issue-type: task
created-at: "2026-06-28T00:10:25.030060+02:00"
---

Files: docs/forth.md, src/core/*.f, test/gate-dictionary.f, docs/stdlib.md. Root cause: include/packages landed first, but structures/enums and the SwiftForth manual sections (timing, string buffers/data, linked lists, switches, execution vectors, exceptions) have not been converted into explicit Habu implementation work. Fix: research the local SwiftForth PDF, create precise child dots, implement the non-speculative Habu language/library features in typed Habu, and validate through native bin/hb gates. Why: language feature work must live in dots and be finished or explicitly scoped, not carried as chat context.
