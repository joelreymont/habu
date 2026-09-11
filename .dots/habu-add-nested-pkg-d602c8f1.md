---
title: Add nested package hierarchy
status: open
priority: 1
issue-type: task
created-at: "2026-07-21T22:36:18.942568+02:00"
---

Invariant: a subsystem owned inside another subsystem must be able to declare a real child package; consumer imports and repeated name prefixes must not impersonate ownership. Current package state is flat: opening package B while package A is active is deliberately rejected, package names containing a qualifier separator are rejected, and using A imports only direct public words from A. This forces nested Maki concerns into the broad MAKI owner or into long pseudo-namespace tails.

Specify and implement hierarchical package identity across the parser, checker, compiler, recovery image, snapshots, ahead-of-time compilation, reflection, and diagnostics. A nested declaration derives one canonical child identity from its lexical parent, reopens only that exact child, qualifies public words unambiguously through the full path, keeps private words inaccessible outside their owner, and restores the parent scope exactly when the child closes. Define whether children may see parent public or private words; the rule must be explicit and must not grant the ownership privileges that using intentionally lacks. Importing a parent must not silently import every child, and importing one child must be lexically bounded with deterministic collision rejection.

Prove nested declaration, reopen, sibling isolation, arbitrary supported depth, full-path lookup, using a child, parent restoration, malformed separators, duplicate or colliding paths, private access rejection, package sealing, throw rollback, generated declarations, fixpoint identity, snapshots, ahead-of-time compilation, and recovery parity. Replace the current positive nesting-rejection regression with the new contract while retaining negatives for illegal ownership transitions. Migrate real hierarchical candidates in separate owner dots and measure dictionary-name bytes, loaded JIT, DATA, CODELEN, and lookup and load time; require no unexplained growth. This capability owns package hierarchy only, not any subsystem migration.
