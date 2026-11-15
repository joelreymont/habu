---
title: "Bar a program's own word by identity"
status: open
priority: 2
issue-type: task
created-at: "2026-11-15T22:22:24.182072+01:00"
---

Problem: UNSAFE-TOK? matches token text, so a program's own private ': ENUM ( -- n ) 5 ;' called bare in its package is E-UNSAFE while its public P:ENUM is accepted; card §1 says a redefinition is checked and called as itself. Acceptance: a program's own word named like a declaration is called as itself; every declaration word is still barred in a checked body. Depends: habu-bar-a-declaration-1ae5a61c. Claim: unassigned.
