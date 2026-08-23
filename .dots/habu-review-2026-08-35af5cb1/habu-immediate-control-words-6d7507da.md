---
title: immediate control words silently shadow package definitions
status: open
priority: 2
issue-type: task
created-at: "2026-08-23T12:11:04.940211+02:00"
---

Problem: a definition whose name is an immediate engine control word compiles silently and then loses at every call site: 'package PKGCASE private : CASE ( -- ) ; : M ( -- ) CASE ;' exits 70 with 'habu: in m: at CASE' (measured 2026-08-23 by the lint lane; FOLD behaves the same), and because the immediate word opened a construct that swallowed the rest of the caller, the diagnostic names the LAST token of the definition, not the guilty one. The reserved-name table (RESTAB, habu2.f:1880-1893) protects package names, not control words. Acceptance: Checker-Miss RCA; defining a name that resolves to an immediate control word is refused at the definition with a named diagnostic (the structural fact exists: the dictionary knows the word is immediate), in both engines (native + bootstrap/cg/forth.fs); a regression fixture per engine; the misplaced-location diagnostic either fixed or recorded with its cause. Files: src/habu/habu2.f (definer publish tail), bootstrap/cg/forth.fs, test/. Verify: the fixture under bin/hb and under the recovery gate. Depends: none. Ownership: reader/definers. Claim: unassigned.
