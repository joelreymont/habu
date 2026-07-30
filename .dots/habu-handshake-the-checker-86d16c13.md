---
title: Handshake the checker package bridge install
status: open
priority: 2
issue-type: task
created-at: "2026-07-30T02:24:15.215498+02:00"
---

Full context: residual gap from the fail-closed fix (agent failclosed 2026-07-30, commit 0a5b92d6). The engine still SILENTLY skips a checker package-scope bridge word that is not resolvable at the call site (src/habu/habu2.f C-FIND-CHECKER shape): a missing checker-end-package leaves the checker's package mirror inside a package the engine already left - a WRONG AUTHORITY rather than a refusal, which no named exit can catch after the fact. Measured during that fix: keying any guard on the-checker-looks-loaded (HOOK-CELL, sibling-name consistency, first-resolution latch) rests on where checker.f defines its bridge words relative to the first package keyword - ordering luck. The structural fix is a real handshake: the checker latches a package-bridge-installed flag into a dedicated engine cell at a defined point in its own source, and the engine refuses package keywords fail-closed while the cell says the bridge is absent (mirroring how DRAIN-PRETRUST anchors the pre-trust handoff at a bare token). Needs new engine layout surface (a cell in the engine-reserved band), the habu2.f keyword-side check, the checker.f latch, mirror parity for bootstrap/cg/forth.fs, and negative regressions: delete checker-end-package from a copied checker.f - the first package keyword must refuse by name, not desynchronize. Related: habu-make-pre-trust-f18dd43a retires the equivalent existence proxy for the pre-trust readiness check; the two handshakes should share the design.
