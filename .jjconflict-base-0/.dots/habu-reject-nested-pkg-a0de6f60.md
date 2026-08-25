---
title: Prove nested package rejection is fail-closed
status: open
priority: 2
issue-type: task
created-at: "2026-07-30T00:00:00.000000+02:00"
---

Full context: rewritten 2026-07-30 after checking the implementation (the first
version of this dot assumed the small engine accepts nested packages; the code
says otherwise, so the task is now to resolve a contradiction between the code
and an observed behavior). The design is settled: packages do NOT nest.
docs/forth.md:222 says package NAME rejects nesting, docs/forth.md:360 requires
fail-closed misuse coverage for nested packages, and the engine implements the
rejection at src/habu/habu2.f:4660-4664 - C-PACKAGE loads PKG-PUB-CELL and, if
a package is already open, takes the $4B wrong-context failure, which is
recoverable inside evaluate and a fail-closed exit at top level. The recently
added nesting feature is `using`/`;using` import scopes inside packages
(USE-DEPTH save/restore at package boundaries), not package-in-package.

The contradiction to resolve: agent snapreloc reported 2026-07-30 that a
`package SNAP-RELOC` opened inside `package SNAP` in src/habu/snap-lib.f
COMPILED on the small-engine load path and only failed later in the snapshot
build (child died E-BUILD-STATUS -2802 after printing a bare "package"). If
that observation is right, some load path swallows the $4B rejection - the
evaluate-recoverable branch caught by something, or the checker's separate
package scope disagreeing with the engine's - and that is a fail-open soundness
gap on the exact path docs/forth.md:360 demands coverage for. If the
observation is wrong, the record must say so.

Task: build the minimal fixture (two-line nested package open) and run it
through BOTH paths: the ordinary top-level `bin/hb --load file.f` and the
snapshot-build child path. Record exit status and stderr for each. If any path
accepts the nested open or fails without the named $4B diagnostic, fix that
path fail-closed and add the negative regression to the package gate coverage
the docs require. If both paths correctly fail closed with the named
diagnostic, record the refutation here with the measured outputs and close with
a negative regression only if the docs-required coverage does not already pin
it (check the existing package misuse gate tests first).
