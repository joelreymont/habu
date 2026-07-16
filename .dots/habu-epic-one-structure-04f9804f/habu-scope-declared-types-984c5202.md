---
title: Scope declared types to packages
status: open
priority: 2
issue-type: task
created-at: "2026-07-16T04:11:05.317260+02:00"
---

Problem: package private/public currently scopes dictionary words, but DEFLINEAR and VALUE-RECORD register bare global type names. A type declared in a private package section remains externally nameable and structurally decomposable/reconstructible in checked code, so packages cannot expose an opaque linear resource handle without a raw-pointer indirection or audited casts. Fix: as part of the unified STRUCTURE/ENUM hard cutover, key every declared nominal/linear/structure type by exact package identity and visibility; private types resolve only while their package is open, public types resolve as PACKAGE:TYPE externally, and recorded public word schemes may carry opaque private types through inference without allowing callers to spell or destructure them. Generate MAKE/UNMAKE/field operations at the same visibility; reject private-type qualification, cross-package aliasing, export laundering, snapshot/AOT/replay visibility drift, and rollback leaks. Acceptance: a private linear STRUCTURE can be returned by PACKAGE:SPAWN and consumed by PACKAGE:WAIT; external inference works, external type spelling/MAKE/UNMAKE/field access rejects, duplication/drop rejects, package reopen works, public structures remain qualified, and bootstrap/native/AOT/snapshot behavior is identical. Files: unified declaration/checker/compiler implementation, package/type registries, verification/replay/source contracts, focused type/package tests, docs/type-families.md and docs/forth.md. Depends: unified STRUCTURE/ENUM compiler lowering and hard-cut migration. Ownership: type identity/visibility only; resource implementations remain consumers. Claim: unassigned.
