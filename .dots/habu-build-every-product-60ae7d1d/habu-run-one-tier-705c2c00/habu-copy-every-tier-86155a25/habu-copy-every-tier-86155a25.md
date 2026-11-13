---
title: Copy every tier-1 package T0 depends on into T0
status: open
priority: 2
issue-type: task
created-at: "2026-11-03T13:55:30.280713+02:00"
---

Goal, Joel 2026-11-03, asked why tier 1 kept being modified: "i want to share frontend code, not build hooks and ties into code we are about to retire!" Settled: T0 shares only front-end code with tier 1. What T0 needs from tier 1's packages is copied into T0's own package and owned there; tier 1 keeps its copy, unedited, until it is deleted. Asked later to rename a word in NDEFINITION, Joel 2026-11-03: "i did not approve any use of NDEFINITION, i object to the use of the N* packages and thought you were redoing them". The direction, not yet a design: review each piece of tier-1 code T0 depends on and copy it into T0 under a proper package and name; copy, review, adjust; do not rewrite. Joel 2026-10-31 expected the front end copied from tier 1: one shared token model and refusal set for both native tiers, separate code generation.

Problem: T0 still calls 15 of tier 1's N* packages from src/compiler/native (rg over src/habu/t0*.f: NEMIT 33, NDICT 20, NHOST 11, NDEFINITION 10, NPUB 8, NSHADOW 7, NCBSTUB 7, NFETCH-CHECK 5, NFETCH 5, NCOMP-DISPATCH 4, NTRAP 2, NFAM 2, NELAB 2, NCOMP 1, NABI 1) and requires their files (src/habu/t0.f:19-34, t0-emit.f:12-13, t0-arm64.f:45-46, t0-x64.f:15-16). NCOMP-DISPATCH is defined in src/habu/layout.f:620, and both tiers write its cells. T0's publish step is its own already (t0.f:2041 PUBLISH-IT). The callback group is replaced, not copied (habu-make-a-cb-6de47af1). T0's fixed-shadow cell, named by Joel NCOMP-DISPATCH:COMP-SHADOW-CELL, is picked by the tier at the builder sites and lands with T0's Wasm emitter, so that a tier-0 create, does> child or defer under an open Wasm shadow keeps its routine (test/wasm/capture.f). This supersedes the keep list in habu-delete-tier-1-f5c44fb4.

Acceptance: a census of every word T0 reaches in each N* package, with its transitive tier-1 dependencies and its other readers (capture, aot-shadow, link, tools); a design naming each new package (names go to Joel); then copy lanes filed as dots, after which T0 calls no N* package.

Claim: unassigned.
