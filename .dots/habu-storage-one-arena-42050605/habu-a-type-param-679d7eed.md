---
title: A type parameter stands for any stored type; [type] states one
status: open
priority: 2
issue-type: task
created-at: "2026-11-11T20:44:44.929295+02:00"
---

Problem: a type parameter stands for a one-cell value and a record never binds one (src/core/checker.f LAYOUT-BLOCK?, docs/forth.md), so no generic word can make or index elements of a stored type and every typed store generates per-declaration words. Design (dot habu-give-an-arena-25a7d3cc, Design 2 decisions 1, 2, 4, 5 and Design 3): a stored-type parameter kind beside TVK-CELL, derived not written, usable only as a pointee or a family argument in a stored slot; the form [type] <type> in a body and at top level yields one cell typed width<T> holding T's stored bytes, refusing what CHECKER-ARENA-INFO refuses and a type variable; width>n reads it; in a CAST: row a variable occurring only as a pointee is a stored-type parameter, so a private ( ptr u8 -- ptr t ) is admitted. Names [type] and width<t> are Joel's. Acceptance: a generic word typed ( ptr u8 width<t> -- ptr t ) loads and checks for n, r, u8, a pointer and a DERIVE addr record, at top level and in a body; tools/check.f accepts the form; a warm build rebuilds a file whose [type] names a changed record. Files: src/core/checker.f, src/habu/interpreter.f, src/core/layout-buffer.f, src/habu/verify-source.f, docs/forth.md, docs/forth-card.md, tests. Verify: new cases in the checker's tests; engine third generation equals the second; suite. Depends: none. Ownership: carl. Claim: unassigned.
