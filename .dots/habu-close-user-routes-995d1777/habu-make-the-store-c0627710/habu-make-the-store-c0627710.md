---
title: Close the checker files to programs
status: active
priority: 2
issue-type: task
created-at: "2026-11-15T19:39:41.033822+01:00"
---

Problem: a program can name every global or public word of src/core/checker.f, check-walk.f, check-hook.f, render.f and top-row.f (519 have callers outside them). Among them: the store writers (E-ADD-EFFECT, E-BUILD-EFFECT, E-ADD-DELETED, RECOVERY-RECORD, USIGS-RESTORE-END, CHECKER-STORAGE-*, CHECKER-UNDEFINE, CHECKER-USIGS-TRUNCATE-FROM, CHECKER-REC-SYM, CHECK:FACTS!, LAY-FACTS, HOLD-RETIRED, GENERATES:), the control- and defer-flag writers (CHECKER-EFFECT-AUTHORITY:PUBLISH, NORET-*, DFER-*), the scope pair CHECKER-SCOPE-START-NEUTRAL and CHECKER-SCOPE-DONE, and TRUST-DECL and TRUST-RAW. INTERPRETER:ALIAS-RECORD (src/habu/interpreter.f) is public and adds a program's alias of a private engine record.
Acceptance: every word of the five files is private to CHECK or its own package, except DEFINITION, CHECK!, CHECK-CANDIDATE!, CHECK-QUIET-CANDIDATE!, CHECK:SCOPED, the words tools and libraries call, and the rejection and verdict constants; an engine caller takes a private word by EXPORT in its private section, a global caller first moving into its file's package; INTERPRETER:ALIAS-RECORD is private, LINKER exporting it; with TRUST-DECL and TRUST-RAW private, TRUST-STALE's name check, E-TRUST-UNRESOLVED and its diagnostic are deleted.
Depends: none. Claim: agent=carl workspace=.jj-ws.noindex/carl-pub.
