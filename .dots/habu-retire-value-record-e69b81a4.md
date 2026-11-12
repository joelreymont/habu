---
title: Retire VALUE-RECORD
status: open
priority: 2
issue-type: task
created-at: "2026-11-07T01:42:19.058693+02:00"
---

Problem: docs/forth.md:1306 calls VALUE-RECORD the legacy by-value record (new records use STRUCTURE; habu-convert-the-type-d5aad352 lists it as migration debt but does not retire it). Nothing outside the tree's own support code declares one: rg 'VALUE-RECORD ' finds no site in src/lib, tools' programs, Tender, Zen, Maki or Etch; its support is ~440 lines (src/core/checker.f VREC-* 357, tools/check-core.f 38, src/habu/verify-source.f 12, type-family.f, roles.f, habu2.f and jit.f references), and 6 test files declare one (engine-suite.f :1054-1095, c2-retained-contract-wide.f, xt-cell-test.f, c2-read-effects.f, type-decl-suite.f, enum-decl-suite.f). Its cells hold plain atoms (VREC-FIELD-WRAP), which made facts text need the `'NAME` atom spelling (C2c-d), and its records hide a type variable the name cannot say (`VALUE-RECORD box value a`). Acceptance: VALUE-RECORD and END-VALUE-RECORD are refused with a named diagnostic and a test; the VREC machinery is deleted; each test that used one to build a wide by-value shape uses a STRUCTURE (or is deleted when it tested VALUE-RECORD itself); docs drop it. habu2.f/jit.f references go only where their feature is deleted. Files: as above. Verify: candidate build; full suite. Depends: C2e (habu-c2e) and the facts text lanes landing first. Ownership: unassigned. Claim: unassigned.
