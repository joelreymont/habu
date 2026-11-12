---
title: Declare a public family inside a package part
status: open
priority: 2
issue-type: task
created-at: "2026-11-03T12:03:39.147281+02:00"
---

Problem: a public family declared in a part cannot be declared or reached. `package PA package PB public STRUCTURE point FIELD x n FIELD y n ;STRUCTURE ;package ;package` (one token per line) is rejected 'bad structure declaration 'point': name is reserved or already taken' (rc 70), and so is a public ENUM of several variants ('bad enum declaration'); a public one-variant ENUM dies 'xref: generated declaration visibility mismatch' (rc 76); a public SUMTYPE ends 'hb: uncaught throw code 7110' (rc 67) with no other line. The constructor package is the cause for public families: TF-CTOR-PKG$ (src/core/type-family.f) gives `PA:PB-POINT`, which no reader resolves and `package` rejects; mapping the colon to `-` would make the words reachable outside PA, and the last segment alone collides with a top-level PB. The design /Users/joel/Work/maki/.jj-ws.noindex/tim/.out/r2/design-structure-in-part.md makes that package a part of PA; its public items go to Joel before any lane.

Acceptance: each of the four cases above works or is rejected by name; a public STRUCTURE declared in two sibling parts under one package loads, and its fields and maker work from inside the parent; the naming rule is stated in docs/forth.md's nested-packages section. Depends: a consumer that declares a public family in a part. Consumers: none; Maki's parts declare only private families. Claim: unassigned.
