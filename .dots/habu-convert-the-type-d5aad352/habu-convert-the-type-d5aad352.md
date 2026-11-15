---
title: "Retire SUMTYPE: one declaration system"
status: open
priority: 1
issue-type: task
created-at: "2026-07-30T16:15:45.305417+02:00"
---

Problem: two declaration systems coexist. The legacy one, src/core/sumtype.f (NEWTYPE and SUMTYPE), sits beside STRUCTURE and ENUM (src/core/structure-decl.f and src/core/enum-decl.f), with the payload and field resolver, the arity parser and the POLICY and DERIVE clause parsers each written more than once. docs/forth.md ("Structures And Enums") calls SUMTYPE, VALUE-RECORD and BEGIN-STRUCTURE migration debt, forbidden in new code. Measured on master 07764450: SUMTYPE is declared in 14 non-test files under src, lib and tools (31 sites) and in 42 files in all; BEGIN-STRUCTURE in 8 non-test files.

Acceptance: every SUMTYPE site is an ENUM or a STRUCTURE; SUMTYPE's front end and grammar are deleted; ENUM and STRUCTURE share one clause parser. A retired spelling is an undefined word, with no diagnostic of its own. NEWTYPE and DEFTYPE stay: docs/forth.md documents both.

Not part of this: binder heads (NAME<a,b>), a carrier NEWTYPE and an owner-construction clause. They were planned with this conversion and dropped (habu-cut-enum-to-b83a478f, habu-checker-sealed-destructure-d967fc03).

Child: habu-nest-generated-family-70b2f31a.

Files: src/core/sumtype.f, src/core/enum-decl.f, src/core/structure-decl.f, the declaration sites. Verify: build, generation convergence and bin/hb --load test/run.f. Depends: none. Ownership: declaration front ends. Claim: unassigned (stale claim reset 2026-10-22).
