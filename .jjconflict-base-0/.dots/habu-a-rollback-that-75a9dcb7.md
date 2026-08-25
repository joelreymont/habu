---
title: A rollback that meets a live floor throws mid-rollback
status: open
priority: 2
issue-type: task
created-at: "2026-08-10T12:47:03.725895+02:00"
---

TRUNCATE now refuses (E-LIVE 7179) a floor at or below the highest surviving routine's start - correct for FORGET, but src/core/generated-declaration-dictionary.f ROLLBACK is a reclamation caller too, and a throw from a rollback participant is a bad place to throw (the transaction is already unwinding). Unreachable today (generated declarations create NEW words; verified the ordinary path cannot trip it - forget-floor lane 2026-08-10) but the failure mode on reachability is an aborted rollback. Decide: prove structurally that a generated-declaration transaction can never span a republication of a pre-existing word (and pin it), or give ROLLBACK a pre-check that answers the question before unwinding begins. Files: src/core/generated-declaration-dictionary.f, src/habu/xref.f. Depends: none.
