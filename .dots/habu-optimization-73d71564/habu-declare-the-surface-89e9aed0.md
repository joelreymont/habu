---
title: "Declare the engine surface in source"
status: open
priority: 2
issue-type: task
created-at: "2026-09-16T17:06:22.524637+03:00"
---

Problem: 3,282 globals and 2,827 package publics ship with names and checker data. From the engine-entry roots, 3,757 of those records are unreachable (110,536 B of code, 35,738 B of records, 38,425 B of names), and their checker data costs 82,330 B (global) and 40,100 B (public). Most of it is compiler and checker plumbing that is global or public only because `checker.f`, `errors.f`, `include.f`, `xref.f` and `repl.f` predate packages. 1,260 of the unnamed globals are used only inside their defining file (`checker.f` alone accounts for 802).
Decision (heron, 2026-09-30): the surface is declared in source with the language's own visibility, and never derived from which files happen to name a word. Rejected: a census that computes the surface from the text of tests and tools. It would make engine bytes depend on test text, and adding a test would widen the engine silently.
Direction: put the legacy global files in packages:
- words used only inside their file become private;
- words other files name become public;
- consumers keep bare spelling through `using`.
The design found that a global which calls a private helper must live inside its package. About 450 bare-called words are named from outside (171 by product consumers, 111 whitebox-only, 163 cross-file engine).
Acceptance: a Fable design pass fixes the per-file plan against the residual measured after 974304d0 and 130fd5d0 land, then this dot is split into dispatchable leaves. Downstream (Tender, Etch, Loom) builds green on the result, with each hit turned into a public API.
Depends: habu-seal-every-captured-c550102f, habu-drop-private-signatures-974304d0.
Parent: habu-ship-only-the-d7d38629. Design: the Fable surface design of 2026-09-30 (~/.cache/tmp/heron-arm64/design-surface.md); census: ~/.cache/tmp/heron-arm64/size-census/.
