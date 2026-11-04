---
title: Share the unit step between NCOMP and T0
status: closed
priority: 2
issue-type: task
created-at: "2026-10-28T21:20:39.952877+02:00"
closed-at: "2026-11-04T11:48:48.471104+02:00"
close-reason: "Census 2026-11-04, done: Landed bdf5ab66 (NDEFINITION); the sharing is now reversed by \"editing tier 1\" (copy into COMP, dot 86155a25)."
---

A1. Move the compiler-neutral unit step out of NCOMP-private src/compiler/native/compiler.f into new src/compiler/native/definition.f, package NDEFINITION, which compiler.f requires. Words per l4-census sec. 1 'Reusable for T0'. definition.f must not require tape.f, feed.f, elaborate.f, backend.f, hir*.f or compiler.f. No behavior change. Workspace .jj-ws.noindex/jerry-a1 on the jerry-a64 merge.

FINAL (jerry-a1 prlmkvsn): review fold-ins applied; gen-1 engine == gen-2; focused rows session, native-create-does, certify-does-definer, ffi-callback-test, native-hookless-reject, does-clause-record, error-code-lint-fixtures all pass (jerry-a1/.out/focusf/).
