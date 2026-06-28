---
title: Implement typed enums
status: closed
priority: 1
issue-type: task
created-at: "\"2026-06-28T00:10:46.958274+02:00\""
closed-at: "2026-06-28T08:04:24.403341+02:00"
close-reason: "completed: ENUM and ENUM4 landed in src/core/enums.f using create/does>, with sequence/package/duplicate coverage in test/gate-dictionary.f and full native gate"
---

Files: src/core/enums.f (new) or the chosen core language file, test/gate-dictionary.f, docs/forth.md, docs/stdlib.md, FILEMAP.md. Root cause: SwiftForth provides ENUM and ENUM4 for sequential constants and named THROW code families, while Habu currently hand-maintains numeric constants and throw codes. Fix: implement typed Habu enum-defining words compatible with the checked compiler path, with ENUM (n -- n+1) and ENUM4 (n -- n+4) semantics using project uppercase names, duplicate/reserved-name rejection, and tests for constants, package scope, case-insensitive duplicate failure, and throw-code usage. Why: enums make named error/status families concise without silent numeric drift.
