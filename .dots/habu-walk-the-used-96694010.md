---
title: Walk the used-publics leg for a staged callee
status: open
priority: 3
issue-type: task
created-at: "2026-08-04T14:54:25.854638+02:00"
---

NMIGRATE:CALLEE resolves a staged callee spelling with the engine's own bare-tail order (open package private, then public, then the global wordlist) plus the qualified leg, in src/compiler/native/migrate.f SPELL-START. The engine's LFIND has one leg more: after the global wordlist it tries each used package's public wordlist (habu1.f LFINDUSED). A migrated body that names its callee through a 'using' therefore resolves in the engine and answers absent here, so the migration is refused with E-NMIGRATE-CALLEE rather than compiled against an unconfirmed address. That is the fail-closed direction and no caller in the tree does it today. Closing this means walking the used-publics leg too - which needs the engine's using state, the same state src/core/checker.f CHECKER-USED-BIND reads - and a case in test/compiler/native-inline.f that migrates a caller inside a 'using' scope.
