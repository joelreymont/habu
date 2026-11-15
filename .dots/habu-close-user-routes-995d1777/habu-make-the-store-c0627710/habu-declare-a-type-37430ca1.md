---
title: Declare a type in a global section
status: active
priority: 2
issue-type: task
created-at: "2026-11-15T22:59:25.006186+01:00"
---

Problem: a SUMTYPE, STRUCTURE or ENUM declared in a package's global section registers with visibility CHECKER-PACKAGE-GLOBAL under the package (generated-declaration.f VIS, sumtype.f TDECL-VIS); the load runs it but tools/check.f rejects the next word E-UNDEFINED-TOP-LEVEL (/tmp/carl-global/p/sum.f). Acceptance: a family in a global section is declared as at top level and resolves its names in the package's scope; load and check agree. Depends: none. Claim: agent=worker workspace=.jj-ws.noindex/carl-gdecl.
