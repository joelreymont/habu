---
title: Fix lookup-symbol crash in stack trace symbolication
status: closed
priority: 1
issue-type: bug
assignee: ""
created-at: "2025-12-08T12:44:43.267732+02:00"
closed-at: "2025-12-08T13:22:18.907948+02:00"
close-reason: ""
---

lookup-symbol crashes when called from print-symbolicated-addr. Symtab is correctly found (count=290), but the lookup function fails. Need to debug the symbol table traversal.
