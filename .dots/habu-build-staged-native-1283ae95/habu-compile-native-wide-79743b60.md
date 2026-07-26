---
title: Compile native wide values
status: open
priority: 1
issue-type: task
created-at: "2026-07-26T22:56:29.072034+02:00"
---

Full context: design Wave 6 under the user's syntax ruling adds wide NEWTYPE/ENUM/STRUCTURE representation lowering, multi-cell layouts, construct/MATCH, wide locals/memory, and linear values. Acceptance: one source parse/HIR build, no pass-2 recompile, representation-witness mutations reject, and existing wide-layout tests use the new compiler.
