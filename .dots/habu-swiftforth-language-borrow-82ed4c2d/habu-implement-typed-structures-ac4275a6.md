---
title: Implement typed structures
status: closed
priority: 1
issue-type: task
created-at: "\"2026-06-28T00:10:39.438484+02:00\""
closed-at: "2026-06-28T08:04:24.337651+02:00"
close-reason: "completed: BEGIN-STRUCTURE/+FIELD/CFIELD:/END-STRUCTURE landed in src/core/structures.f with typed ptr-a/ptr-u8 field access, positive layout tests, malformed/nested misuse tests, byte-field checker reject, docs, and full native gate"
---

Files: src/core/structures.f (new), src/habu/habu2.f/bootstrap prelude if language support is needed, src/core/checker.f if nominal field typing is required, test/gate-dictionary.f, docs/forth.md, docs/stdlib.md, FILEMAP.md, TRUSTED.md. Root cause: Habu has ad hoc ptr-field helpers but no Forth structure DSL, and user-requested syntax BEGIN-STRUCTURE POINT / CELL +FIELD POINT.X / CFIELD: POINT.FLAGS / END-STRUCTURE must typecheck field access instead of relying on manual stack juggling. Fix: implement small checked words for structure layout and field accessors, reject malformed/nested structures fail-closed, model field access in the checker (generic ptr a at minimum; nominal struct/field type if current checker can express it), add positive/negative native tests, and document syntax. Why: typed structure fields are a core Forth capability and eliminate repeated untyped offset plumbing.
