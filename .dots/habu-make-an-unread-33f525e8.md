---
title: Make an unread register constraint impossible
status: open
priority: 2
issue-type: task
created-at: "2026-07-31T18:12:27.823271+02:00"
---

src/compiler/native/regalloc.f reads exactly two declarations about a form: the register class of each value (its type) and the ties its schema declares. That is the whole constraint vocabulary today, so reading those two is reading everything. Nothing structural stops a later schema field - a fixed register, a clobber list, a second register class - from being added without A64RA reading it: the allocator would then allocate around a constraint it cannot see, which is exactly the failure the tie declaration was moved into the schema to prevent (the old E-A64RA-OPCODE refusal used to catch an unmodelled FORM, and it is retired). Make it structural: either a schema-level constraint vocabulary A64RA must MATCH exhaustively, so a new constraint kind fails to compile until the allocator answers for it, or a per-dialect constraint-version the allocator pins and IR-SCHEMA bumps when the vocabulary grows. Owners: IR-SCHEMA, A64RA.
