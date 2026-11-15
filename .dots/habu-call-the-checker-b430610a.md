---
title: Call the checker by name, not through the owner record
status: open
priority: 2
issue-type: task
created-at: "2026-11-15T17:07:14.193492+01:00"
---

Problem: src/core/checker.f DECLARATIONS is a record of checker words that the engine and tools call through NCOMP-DISPATCH:DECL-A (the image's own checker; a probe through an engine build, the whole suite and a rejected definition found no other) and TARGET-DECL-A (a checker a build is loading). Callers: src/habu/interpreter.f, habu1.f and habu2.f (and the calls they emit), xref.f, runtime.f, data-claims.f, verify-source.f; src/core/include.f, structures.f, sumtype.f; src/core/check-hook.f's swaps of TRUST-RAW, TRUST-DECL, CHECKER-DEFER, CHECKER-DEFCAST and CHECKER-LINEAR; tools/lsp-defs.f. docs/design.md 'Machinery being replaced' replaces a hook with a direct call or data the owner records; src/compiler/native/checker-owner.f already calls the checker by name. Comments at src/core/checker.f ENTER-SECTION and src/habu/interpreter.f PKG-TARGET say an engine reads a loading checker's record through TARGET-DECL-A; a review found every store into DECL-CELL and TARGET-DECL-CELL routed into the image in a build (src/habu/habu2.f ROUTE-HEADER), so each engine reads only its own record. Which holds decides whether the record's freed cells may close up. Acceptance: each caller calls the checker word by name, or the record keeps only what a build must take from the checker it is loading, with that reason at the record; dots aee1f624, 0ae7be34 and ae6ad495 are closed or rewritten to match. Depends: none. Claim: unassigned.
