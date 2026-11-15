---
title: "Move xref.f's checker callers into package XREF"
status: active
priority: 2
issue-type: task
created-at: "2026-11-15T21:44:40.294771+01:00"
---

Problem: src/habu/xref.f's global UNDEFINE-NAME, UNDEFINE-FOUND (CHECKER-UNDEFINE), HIDE-DEFS-FROM, FORGET-DEFS-FROM (CHECKER-USIGS-TRUNCATE-FROM-RAW) and DOES-COMPANION-INSTALL (DOES-COMPANION?-XT) call checker words; once the checker files go private, global words cannot reach them. Acceptance: the five live in package XREF (the installer private), their callers name them XREF:NAME, undefine and UNDEFINE-IF-DEFINED stay global; the engine builds and the suites that call them pass. Depends: none. Claim: agent=worker workspace=.jj-ws.noindex/carl-d4-xref.
